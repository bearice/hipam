{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where
import System.Directory (removeFile)
import System.Environment
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Network.Socket
import Data.Maybe
import Data.List
import Data.Default.Class (def)
import Control.Exception (bracket)

data ServerSocketAddress = UnixSocket String | TCPSocket Int

main :: IO()
main = bracket createSocket destroySocket $ \(_,socket) -> do
    listen socket 10
    scottySocket def socket routing
  where
    listenOn = do
      env <- getEnvironment
      let val = fromMaybe "./ipam.sock" $ lookup "LISTEN" env
      return $ if "unix://" `isPrefixOf` val
        then
          addrInfo AF_UNIX $ SockAddrUnix $ drop 7 val
        else
          addrInfo AF_INET $ SockAddrInet (fromInteger $ read val) 0
        where
          addrInfo af addr =
            AddrInfo { addrFamily = af, addrAddress = addr,
            addrSocketType =Stream, addrFlags = [],
            addrProtocol = 0, addrCanonName = Nothing }

    createSocket = do
      addr <- listenOn
      print addr
      sock <- socket (addrFamily addr) Stream 0
      bind sock (addrAddress addr)
      return (addr,sock)

    destroySocket (addr,sock) = do
      address <- listenOn
      close sock
      case addrAddress address of
        SockAddrUnix path -> removeFile path
        _ -> return ()

    routing = do
      middleware logStdoutDev
      get "/" $ text "Hello, world!"
