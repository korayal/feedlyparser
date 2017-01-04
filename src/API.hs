{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module API
  ( app
  , FeedlyAPI) where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (runExcept)
import           Data.Aeson
import           Data.List                  (sortBy)
import           Servant
import           Servant.API
import           Servant.Server             (Application, Server, serve)
import           Types                      (FeedItem, FeedList, engagement,
                                             feedList, items, title)

type FeedlyAPI = "hn" :> "list" :> Get '[JSON] FeedList
            :<|> "hn" :> "list" :> Capture "minEngagement" Integer :> Get '[JSON] [(Integer, FeedItem)]
            :<|> "hn" :> "list" :> "titles" :> Get '[JSON] [String]

feedlyServer :: Server FeedlyAPI
feedlyServer = feedlist :<|> titleEngagement :<|> titlesOnly
  where
    feedlist = do
      fle <- (liftIO . feedList) "hn.json"
      return $ case runExcept fle of
        Left _   -> error "couldn't parse the file"
        Right fl -> fl

    titlesOnly = do
      fl <- feedlist
      return $ map title (items fl)

    titleEngagement a = do
      fl <- feedlist
      return $ (
          map (\x -> (engagement x, x))
          . filter (\x -> engagement x > a)
          . sortBy (\x y -> (engagement y) `compare` (engagement x))
        ) (items fl)

feedlyAPI :: Proxy FeedlyAPI
feedlyAPI = Proxy

app :: Application
app = serve feedlyAPI feedlyServer
