{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Util where

import Web.Scotty.Trans
import Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Aeson hiding (json)
import Data.Aeson.Types (emptyObject)

mkJson :: (ScottyError e, Monad m) => [(T.Text, Value)] -> ActionT e m ()
mkJson = json . Object . fromList

mkError :: (ScottyError e, Monad m) => Value -> ActionT e m ()
mkError err = mkJson [("Err",err)]

mkEmptyObj :: (ScottyError e, Monad m) => ActionT e m ()
mkEmptyObj = json emptyObject
