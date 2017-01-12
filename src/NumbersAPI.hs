{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module NumbersAPI
  ( NumbersAPI
  , numbersServer) where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Monad.Reader
import           Servant
import           Servant.API
import qualified Servant.Server              as SS
-- Local Libs
import qualified Combo                       as C

type NumbersAPI = "numbers" :> (
    Get '[JSON] [Integer]
    :<|> Capture "number" Integer :> Put '[JSON] NoContent
    :<|> Capture "number" Integer :> Delete '[JSON] NoContent
    :<|> ReqBody '[JSON] [Integer] :> Post '[JSON] NoContent
  )

numbersServer
    :: (MonadReader C.ComboState m, MonadIO m)
    => SS.ServerT NumbersAPI m
numbersServer = listNumbers :<|> putNumber :<|> deleteNumber :<|> addBulkNumbers
  where
    modifyWith f = do
      il' <- ask
      (liftIO . atomically . modifyTVar (C._nl il')) f
    listNumbers = ask >>= (liftIO . readTVarIO . C._nl)
    putNumber newNumber = modifyWith (newNumber :) >> return NoContent
    deleteNumber newNumber = modifyWith (filter (/= newNumber)) >> return NoContent
    addBulkNumbers numbers = modifyWith (numbers ++) >> return NoContent
