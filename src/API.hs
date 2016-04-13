{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module API where
import Control.Monad
import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Reader

import Data.Maybe
import Data.List
import Data.Default.Class
--import Web.Scotty
import Web.Scotty.Trans
import Network.Wai.Middleware.RequestLogger
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Aeson hiding (json)
import Data.Aeson.Types (emptyObject)
--import Data.Vector as V
import Data.HashMap.Strict as M
import Data.Text.Format
import Data.Word
import Data.IP

data AppState = AppState {
    tickCount :: Int,
    nextAddr :: Word32
  }

instance Default AppState where
    def = AppState 0 $ byteSwap32 $ toHostAddress (read "10.16.0.1")

newtype WebM a = WebM { runWebM :: ReaderT (TVar AppState) IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

-- Some helpers to make this feel more like a state monad.
gets :: (AppState -> b) -> WebM b
gets f = liftM f (ask >>= liftIO . readTVarIO)

modify :: (AppState -> AppState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

mkState :: IO (TVar AppState)
mkState = newTVarIO def

withState :: TVar AppState -> WebM a -> IO a
withState s m = runReaderT (runWebM m) s

routing :: ScottyT L.Text WebM ()
routing = do
  middleware logStdoutDev
  get  "/" $ do
    webM $ modify $ \ st -> st { tickCount = tickCount st + 1 }
    c <- webM $ gets tickCount
    text $ format "Hello, world! c={}" $ Only c
    webM $ modify $ \ st -> st { tickCount = tickCount st + 1 }

  post "/Plugin.Activate" (mkJson [
      ("Implements", Array ["NetworkDriver","IpamDriver"])
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

  post "/IpamDriver.ReleasePool" $ json emptyObject

  post "/IpamDriver.RequestAddress" $ do
    next <- webM $ do
      modify $ \st -> st { nextAddr = nextAddr st + 1 }
      gets $ byteSwap32.nextAddr

    mkJson [
      ("Address", String $ T.pack $ mconcat [show $ fromHostAddress next, "/24"]),
      ("Data", emptyObject)]

  post "/IpamDriver.ReleaseAddress" $ json emptyObject

  notFound $ mkError "not found"

mkJson :: [(T.Text, Value)] -> ActionT L.Text WebM ()
mkJson = json . Object . fromList

mkError :: Value -> ActionT L.Text WebM ()
mkError err = mkJson [("Err",err)]
