{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where
import System.Directory (removeFile)
import System.Environment
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Network.Socket
import Data.Default.Class (def)
import Control.Exception (bracket)

main :: IO()
main = bracket createSocket destroySocket $ \socket -> do
    listen socket 10
    scottySocket def socket routing
  where
    sockPath = do
        env <- getEnvironment
        return $ maybe "./ipam.sock" id $ lookup "LISTEN" env

    createSocket = do
        path <- sockPath
        sock <- socket AF_UNIX Stream 0
        bind sock $ SockAddrUnix path
        return sock

    destroySocket sock = do
        path <- sockPath
        removeFile path
        close sock

    routing = do
        middleware logStdoutDev
        get "/" $ text "Hello, world!"


