{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
module NetworkDriver where
import Web.Scotty.Trans as Web
import Util
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import GHC.Generics
import Data.Aeson as JSON
import Data.Aeson.Types
import Data.Map
import Data.ByteString.Lazy
import State
import Debug.Trace

data CreateEndpointRequest = CERequest {
      networkID  :: T.Text
    , endpointID :: T.Text
    , options    :: Map T.Text Value
    , interface  :: Map T.Text T.Text
  } deriving (Generic, Show)

instance FromJSON CreateEndpointRequest where
  parseJSON = fromCamalJSON

data JoinResponse = Join {
      interfaceName :: Map T.Text T.Text
    , gateway       :: T.Text
    , gatewayIPv6   :: T.Text
    , staticRoutes  :: Value
  } deriving (Generic, Show)

instance ToJSON JoinResponse where
  toJSON = toCamalJSON

handler :: ScottyT L.Text WebM ()
handler = do
  post "/NetworkDriver.GetCapabilities" $ mkJson [("Scope","local")]
  post "/NetworkDriver.CreateNetwork" mkEmptyObj
  post "/NetworkDriver.DeleteNetwork" mkEmptyObj
  post "/NetworkDriver.CreateEndpoint" $ do
    i <- fmap interface jsonData
    traceShowM i
    Web.json i
  post "/NetworkDriver.DeleteEndpoint" mkEmptyObj
  post "/NetworkDriver.EndpointOperInfo" $ mkJson [("Value",emptyObject)]
  post "/NetworkDriver.Join" $ mkError "not impemenentd"
  post "/NetworkDriver.Leave" $ mkError "not impemenentd"
