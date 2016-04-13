{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
module NetworkDriver where
import Web.Scotty.Trans
import Util
import Control.Monad.IO.Class

handler :: (ScottyError e, Monad m, MonadIO m) => ScottyT e m ()
handler = do
  post "/NetworkDriver.GetCapabilities" $ mkJson [
      ("Scope","local")
    ]
  post "/NetworkDriver.CreateNetwork" mkEmptyObj
  post "/NetworkDriver.DeleteNetwork" mkEmptyObj
  post "/NetworkDriver.CreateEndpoint" $ mkError "not impemenentd"
  post "/NetworkDriver.DeleteEndpoint" $ mkError "not impemenentd"
  post "/NetworkDriver.EndpointOperInfo" $ mkError "not impemenentd"
  post "/NetworkDriver.Join" $ mkError "not impemenentd"
  post "/NetworkDriver.Leave" $ mkError "not impemenentd"
