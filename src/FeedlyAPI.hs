{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module FeedlyAPI
  ( FeedlyAPI
  , feedlyServer) where

import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader
import           Data.List                   (sortBy)
import           Servant
import           Servant.API
import qualified Servant.Server              as SS
-- Local Libs
import qualified Combo                       as C
import           Types                       (FeedItem, FeedList, engagement,
                                              items, title)
type FeedlyAPI = "hn" :> "list" :> (
    Get '[JSON] FeedList
    :<|> "titles" :> Get '[JSON] [String]
    :<|> Capture "minEngagement" Integer :> Get '[JSON] [(Integer, FeedItem)]
  )

feedlyServer
    :: (MonadReader C.ComboState m, MonadIO m)
    => SS.ServerT FeedlyAPI m
feedlyServer = feedlist :<|> titlesOnly :<|> titleEngagement
  where
    feedlist = do
      cs <- ask
      fl <- (liftIO . readTVarIO . C._fl) cs
      return fl
    titlesOnly = feedlist >>= (return . map title . items)
    titleEngagement a = feedlist >>= (
          return
          . map (\x -> (engagement x, x))
          . filter (\x -> engagement x > a)
          . sortBy (\x y -> (engagement y) `compare` (engagement x))
          . items
        )
