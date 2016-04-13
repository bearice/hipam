{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
module IpamDriver where
import Web.Scotty.Trans
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.Aeson.Types (emptyObject)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Text.Format
import Util
import State

handler :: ScottyT L.Text WebM ()
handler = do
  post "/IpamDriver.GetCapabilities" $ mkJson [
      ("RequiresMACAddress", Bool True)
    ]

  post "/IpamDriver.GetDefaultAddressSpaces" $ mkJson [
      ("LocalDefaultAddressSpace","10.16.0.0/16"),
      ("GlobalDefaultAddressSpace","10.16.0.0/16")
    ]

  post "/IpamDriver.RequestPool" $
    mkJson [
      ("PoolID", "123"),
      ("Pool",   "10.16.0.0/24"),
      ("Data",   emptyObject)]

  post "/IpamDriver.ReleasePool" mkEmptyObj

  post "/IpamDriver.RequestAddress" $ do
    next <- incrAndGetNextAddr
    let addr = String $ L.toStrict.format "{}/24" $ Only $ show next
    mkJson [
      ("Address", addr),
      ("Data", emptyObject)]

  post "/IpamDriver.ReleaseAddress" mkEmptyObj

strcat = String . T.pack . mconcat
