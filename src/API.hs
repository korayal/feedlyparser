{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module API
  ( app
  , FeedlyAPI) where

import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVarIO,
                                              readTVar, readTVarIO)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Except  (Except, except, runExcept)
import           Data.Aeson
import qualified Data.ByteString.Lazy        as B
import           Data.List                   (sortBy)
import           Servant
import           Servant.API
import           Servant.Server              (Application, Server, serve)
import           System.IO.Unsafe            (unsafeInterleaveIO,
                                              unsafePerformIO)
import           Types                       (FeedItem, FeedList, engagement,
                                              items, title)
-- FEEDLY API
type FeedlyAPI = "hn" :> "list" :> (
    Get '[JSON] FeedList
    :<|> "titles" :> Get '[JSON] [String]
    :<|> Capture "minEngagement" Integer :> Get '[JSON] [(Integer, FeedItem)]
  )

feedList :: FilePath -> IO (TVar FeedList)
feedList p = do
  f <- B.readFile p
  case eitherDecode f of
    Left _   -> error "input file is not a valid json"
    Right fl -> (newTVarIO fl)

fl :: TVar FeedList
fl = (unsafePerformIO . unsafeInterleaveIO) (feedList "hn.json")

feedlyServer :: Server FeedlyAPI
feedlyServer = feedlist :<|> titlesOnly :<|> titleEngagement
  where
    feedlist = liftIO (readTVarIO fl)
    titlesOnly = feedlist >>= (return . map title . items)
    titleEngagement a = feedlist >>= (
          return
          . map (\x -> (engagement x, x))
          . filter (\x -> engagement x > a)
          . sortBy (\x y -> (engagement y) `compare` (engagement x))
          . items
        )

-- Numbers API
type NumbersAPI = "numbers" :> (
    Get '[JSON] [Integer]
    :<|> Capture "number" Integer :> Put '[JSON] NoContent
    :<|> Capture "number" Integer :> Delete '[JSON] NoContent
    :<|> ReqBody '[JSON] [Integer] :> Post '[JSON] NoContent
  )

il :: TVar [Integer]
il = (unsafePerformIO . unsafeInterleaveIO) (newTVarIO ([] :: [Integer]))

numbersServer :: Server NumbersAPI
numbersServer = listNumbers :<|> putNumber :<|> deleteNumber :<|> addBulkNumbers
  where
    listNumbers = (liftIO . readTVarIO) il
    putNumber newNumber = (liftIO . atomically . modifyTVar il) (newNumber :) >> return NoContent
    deleteNumber newNumber = (liftIO . atomically . modifyTVar il) (filter (/= newNumber)) >> return NoContent
    addBulkNumbers numbers = (liftIO . atomically . modifyTVar il) (numbers ++) >> return NoContent

type AppAPI = FeedlyAPI :<|> NumbersAPI

api :: Proxy AppAPI
api = Proxy

appServer :: Server AppAPI
appServer = feedlyServer :<|> numbersServer

app :: Application
app = serve api appServer
