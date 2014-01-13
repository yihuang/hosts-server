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
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

type Host = (Domain, IPv4)

data Conf = Conf
  { bufSize     :: Int
  , timeOut     :: Int
  , nameservers :: [HostName]
  , hosts       :: [Host]
  }

instance Default Conf where
    def = Conf
      { bufSize     = 512
      , timeOut     = 10 * 1000 * 1000
      , nameservers = []
      , hosts       = []
      }

toEither :: a -> Maybe b -> Either a b
toEither a = maybe (Left a) Right

{--
 - Proxy dns request to a real dns server.
 -}
proxyRequest :: Conf -> HostName -> DNSFormat -> IO (Either String DNSFormat)
proxyRequest Conf{..} server req = do
    let rc = defaultResolvConf { resolvInfo = RCHostName server }
        worker Resolver{..} = do
            let packet = B.concat . BL.toChunks $ encode req
            sendAll dnsSock packet
            receive dnsSock
    rs <- makeResolvSeed rc
    withResolver rs $ \r ->
        (>>= check) . toEither "proxy request timeout" <$> timeout timeOut (worker r)
  where
    ident = identifier . header $ req
    check :: DNSFormat -> Either String DNSFormat
    check rsp = let hdr = header rsp
                in  if identifier hdr == ident
                        then Right rsp
                        else Left "identifier not match"

{--
 - Handle A query for domain suffixes configured, and proxy other requests to real dns server.
 -}
handleRequest :: Conf -> DNSFormat -> IO (Either String DNSFormat)
handleRequest conf req =
    case lookupHosts of
        (Just rsp) -> return $ Right rsp
        Nothing  -> maybe
                    (return $ Left "nameserver not configured.")
                    (\srv -> proxyRequest conf srv req)
                    (listToMaybe (nameservers conf))
  where
    filterA = filter ((==A) . qtype)
    filterHost dom = filter (\(h, _) -> h `B.isSuffixOf` dom)
    ident = identifier . header $ req
    lookupHosts :: Maybe DNSFormat
    lookupHosts = do
        q <- listToMaybe . filterA . question $ req
        (_, ip) <- listToMaybe . filterHost (qname q) $ hosts conf
        return $ responseA ident q ip

{--
 - Parse request and compose response.
 -}
handlePacket :: Conf -> Socket -> SockAddr -> B.ByteString -> IO ()
handlePacket conf@Conf{..} sock addr s =
    either
    (putStrLn . ("decode fail:"++))
    (\req -> do
        handleRequest conf req >>=
            either
            putStrLn
            (\rsp -> let packet = B.concat . BL.toChunks $ encode rsp
                     in  timeout timeOut (sendAllTo sock packet addr) >>=
                         maybe (putStrLn "send response timeout") return
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

{--
 - parse config file.
 -}
readHosts :: FilePath -> IO ([Host], [HostName])
readHosts filename =
    B.readFile filename >>= either (fail . ("parse hosts fail:"++)) return . parseHosts
  where
    parseHosts :: B.ByteString -> Either String ([Host], [HostName])
    parseHosts s = let (serverLines, hostLines) = partition (B.isPrefixOf "nameserver") (B.lines s)
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
        B.unpack <$> takeWhile (not . isSpace)

main :: IO ()
main = do
    args <- getArgs
    (hosts, servers) <- readHosts $ fromMaybe "./hosts" (listToMaybe args)
    print (hosts, servers)
    run def{hosts=hosts, nameservers=servers}
