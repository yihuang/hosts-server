{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

import Prelude hiding (takeWhile)
import Control.Applicative ( (<$>), (<*>) )
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.Default (Default(def))
import Data.IP (IPv4, toIPv4)
import Data.List (partition)
import Data.Maybe
import System.Environment (getArgs)
import System.Timeout (timeout)
import Network.Socket.ByteString (sendAll, sendAllTo, recvFrom)
import Network.Socket hiding (recvFrom)
import Network.DNS
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as BL

type Host = (Domain, IPv4)

data Conf = Conf
  { bufSize :: Int
  , timeOut :: Int
  , nameservers :: [HostName]
  , hosts   :: [Host]
  }

instance Default Conf where
    def = Conf
      { bufSize = 512
      , timeOut = 10 * 1000 * 1000
      , nameservers = ["192.168.1.1"]
      , hosts   = [("localhost.", "127.0.0.1")]
      }

{--
 - Timeout with error message.
 -}
timeout' :: String -> Int -> IO a -> IO (Maybe a)
timeout' msg tm io = do
    result <- timeout tm io
    maybe (putStrLn msg) (\_ -> return ()) result
    return result

{--
 - Proxy dns request to a real dns server.
 -}
proxyRequest :: Conf -> ResolvConf -> DNSFormat -> IO (Maybe DNSFormat)
proxyRequest Conf{..} rc req = do
    let worker Resolver{..} = do
            let packet = S.concat . BL.toChunks $ encode req
            sendAll dnsSock packet
            receive dnsSock dnsBufsize
    rs <- makeResolvSeed rc
    withResolver rs $ \r ->
        (>>= check) <$> timeout' "proxy request timeout" timeOut (worker r)
  where
    ident = identifier . header $ req
    check :: DNSFormat -> Maybe DNSFormat
    check rsp = let hdr = header rsp
                in  if identifier hdr == ident
                        then Just rsp
                        else Nothing

{--
 - Handle A request configured in hosts, and proxy other requests to real dns server.
 -}
handleRequest :: Conf -> ResolvConf -> DNSFormat -> IO (Maybe DNSFormat)
handleRequest conf@Conf{hosts=hosts} rc req =
    maybe
    (proxyRequest conf rc req)
    (return . Just)
    mResponse
  where
    filterA = filter ((==A) . qtype)
    filterHost dom = filter (\(h, _) -> h `S.isSuffixOf` dom)
    ident = identifier . header $ req
    mResponse = do
        q <- listToMaybe . filterA . question $ req
        (_, ip) <- listToMaybe . filterHost (qname q) $ hosts
        return $ responseA ident q ip

{--
 - Parse request and compose response.
 -}
handlePacket :: Conf -> Socket -> SockAddr -> S.ByteString -> IO ()
handlePacket conf@Conf{..} sock addr s =
    either
    (putStrLn . ("decode fail:"++))
    (\req -> do
        let rc = defaultResolvConf { resolvInfo = RCHostName (head nameservers) }
        handleRequest conf rc req >>=
            maybe
            (return ())
            (\rsp -> let packet = S.concat . BL.toChunks $ encode rsp
                     in  timeout' "send response timeout" timeOut (sendAllTo sock packet addr)
                         >> return ()
            )
    )
    (decode (BL.fromChunks [s]))

run :: Conf -> IO ()
run conf = withSocketsDo $ do
    addrinfos <- getAddrInfo
                   (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                   Nothing (Just "domain")
    addrinfo <- maybe (fail "no addr info") return (listToMaybe addrinfos)
    sock <- socket (addrFamily addrinfo) Datagram defaultProtocol
    bindSocket sock (addrAddress addrinfo)
    forever $ do
        (s, addr) <- recvFrom sock (bufSize conf)
        forkIO $ handlePacket conf sock addr s

readHosts :: FilePath -> IO ([Host], [HostName])
readHosts filename =
    S.readFile filename >>= either (fail . ("parse hosts fail:"++)) return . parseHosts
  where
    parseHosts :: S.ByteString -> Either String ([Host], [HostName])
    parseHosts s = let (serverLines, hostLines) = partition (S.isPrefixOf "nameserver") (S.lines s)
                   in  (,) <$> mapM (parseOnly host) hostLines
                           <*> mapM (parseOnly nameserver) serverLines

    host :: Parser Host
    host = do
        skipSpace
        ip <- toIPv4 . map read <$> (many1 digit `sepBy` string ".")
        _ <- space
        skipSpace
        dom <- takeWhile (not . isSpace)
        skipSpace
        return (dom, ip)

    nameserver :: Parser HostName
    nameserver = do
        _ <- string "nameserver"
        _ <- space
        skipSpace
        S.unpack <$> takeWhile (not . isSpace)

main :: IO ()
main = do
    args <- getArgs
    (hosts, servers) <- readHosts $ fromMaybe "./hosts" (listToMaybe args)
    print (hosts, servers)
    run def{hosts=hosts, nameservers=servers}
