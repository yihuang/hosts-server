What's this
===========

This is a simple dns server, you can config it like config hosts file. e.g. with this line in your config file::

    127.0.0.1 dev.com.

then all the A queries for domain `*.dev.com` will return `127.0.0.1`.

For all the other requests, including non-A requests and A requests for domains which is not configured, it will proxy them to a real dns server, you can config these servers like this: ::

    nameserver 8.8.8.8

At last, an complete config file: ::

    nameserver 8.8.8.8
    nameserver 8.8.4.4
    127.0.0.1 product.com.

Usage
=====

::

    hosts-server /your/path/to/config

The only argument is the path to your config file, default to "./hosts".
