{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}

module API where
import Control.Monad
import Data.Maybe
import Data.List
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Data.Aeson hiding (json)
import Data.Vector as V
import Data.HashMap.Strict as M

routing :: ScottyM()
routing = do
  middleware logStdoutDev
  get  "/" (text "Hello, world!")

  post "/Plugin.Activate" (mkJson [
      ("Implements", Array ["NetworkDriver"])
    ])

  post "/NetworkDriver.GetCapabilities" (mkJson [
      ("Scope","local")
    ])

  post "/NetworkDriver.CreateNetwork" $ mkError "not impemenentd"
  post "/NetworkDriver.DeleteNetwork" $ mkError "not impemenentd"
  post "/NetworkDriver.CreateEndpoint" $ mkError "not impemenentd"
  post "/NetworkDriver.DeleteEndpoint" $ mkError "not impemenentd"
  post "/NetworkDriver.EndpointOperInfo" $ mkError "not impemenentd"
  post "/NetworkDriver.Join" $ mkError "not impemenentd"
  post "/NetworkDriver.Leave" $ mkError "not impemenentd"

  notFound $ mkError "not found"

mkJson = json . Object . M.fromList
mkError err = mkJson [
    ("Err",err)
  ]
