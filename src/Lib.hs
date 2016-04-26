{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( feedList,
      FeedList(..),
      FeedItem(..)
    ) where

import Data.Aeson
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Lazy as B

data FeedItem = FeedItem
    { title :: String
    , engagement :: Integer
    , published :: UTCTime
    , crawled :: UTCTime
    } deriving ((((Show))))

data FeedList = FeedList { items :: [FeedItem]} deriving (Show)

instance FromJSON FeedItem where
  parseJSON = withObject "Feed Item" $ \o ->
                FeedItem <$> o .: "title"
                         -- set engagement to 0 if empty
                         <*> o .:? "engagement" .!= 0
                         <*> (posixSecondsToUTCTime . (/ 1000) . fromInteger <$> o .: "published")
                         <*> (posixSecondsToUTCTime . (/ 1000) . fromInteger <$> o .: "crawled")


instance FromJSON FeedList where
  parseJSON = withObject "Feed List" $ \o ->
                FeedList <$> o .: "items"

feedList :: FilePath -> IO (Either String FeedList)
feedList p = eitherDecode <$> B.readFile p
