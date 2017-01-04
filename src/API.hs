{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API
  ( app
  , FeedlyAPI) where

import Types (FeedList, feedList)
import Data.Aeson
import Servant
import Servant.API
import Servant.Server (serve, Server, Application)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExcept)

type FeedlyAPI = "hn" :> "list" :> Get '[JSON] FeedList

feedlyServer :: Server FeedlyAPI
feedlyServer = do
  fle <- (liftIO . feedList) "hn.json"
  return $ case runExcept fle of
    Left _ -> error "couldn't parse the file"
    Right fl -> fl

feedlyAPI :: Proxy FeedlyAPI
feedlyAPI = Proxy

app :: Application
app = serve feedlyAPI feedlyServer
