{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
module API where
import Control.Monad
import Web.Scotty.Trans
import Network.Wai.Middleware.RequestLogger
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Text.Format
import Data.Aeson hiding (json)
import Data.Aeson.Types (emptyObject)
import State
import Util
import NetworkDriver as ND
import IpamDriver as ID

routing :: ScottyT L.Text WebM ()
routing = do
  middleware logStdoutDev
  get  "/" $ do
    c <- webM $ modifyAndGet $ \ st -> st { tickCount = tickCount st + 1 }
    text $ format "Hello, world! c={}" $ Only $ tickCount c
    webM $ modify $ \ st -> st { tickCount = tickCount st + 1 }

  post "/Plugin.Activate" (mkJson [
      ("Implements", Array ["NetworkDriver","IpamDriver"])
    ])

  ND.handler
  ID.handler
  notFound $ mkError "not found"
