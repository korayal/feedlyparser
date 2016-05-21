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

data FeedAlternate = FeedAlternate
  { href :: String
  , _type :: String
  } deriving (Show)

newtype FeedAlternateList = FeedAlternateList { alternateList :: [FeedAlternate] } deriving (Show)

instance FromJSON FeedAlternate where
  parseJSON = withObject "Feed Alternate" $ \o ->
                FeedAlternate <$> o .: "href"
                              <*> o .: "type"

data FeedOrigin = FeedOrigin
  { streamId :: String
  , foTitle :: String
  , htmlUrl :: String
  } deriving (Show)

instance FromJSON FeedOrigin where
  parseJSON = withObject "Feed Origin" $ \o ->
                FeedOrigin <$> o .: "streamId"
                           <*> o .: "title"
                           <*> o .: "htmlUrl"

data FeedSummary = FeedSummary
  { content :: String
  , direction :: String
  } deriving (Show)

instance FromJSON FeedSummary where
  parseJSON = withObject "Feed Summary" $ \o ->
                FeedSummary <$> o .: "content"
                            <*> o .: "direction"

data FeedVisual = FeedVisual
  { url :: String
  , width :: Integer
  , height :: Integer
  , processor :: String
  , contentType :: String
  } deriving (Show)

instance FromJSON FeedVisual where
  parseJSON = withObject "Feed Visual" $ \o ->
                FeedVisual <$> o .: "url"
                           <*> (fromInteger <$> o .:? "width" .!= 0)
                           <*> (fromInteger <$> o .:? "height" .!= 0)
                           <*> (o .:? "processor" .!= "")
                           <*> (o .:? "contentType" .!= "")

data FeedCategory = FeedCategory
  { fcid :: String
  , fclabel :: String
  } deriving (Show)

newtype FeedCategoryList = FeedCategoryList { categoryList :: [FeedCategory] } deriving (Show)

instance FromJSON FeedCategory where
  parseJSON = withObject "Feed Category" $ \o ->
                FeedCategory <$> o .: "id"
                             <*> o .: "label"

data FeedItem = FeedItem
    { id :: String
    , originId :: String
    , fingerprint :: String
    , title :: String
    , published :: UTCTime
    , crawled :: UTCTime
    , alternate :: [FeedAlternate]
    , origin :: FeedOrigin
    , summary :: FeedSummary
    , visual :: Maybe FeedVisual
    , unread :: Bool
    , categories :: [FeedCategory]
    , engagement :: Integer
    , engagementRate :: Integer
    } deriving (Show)

instance FromJSON FeedItem where
  parseJSON = withObject "Feed Item" $ \o ->
                FeedItem <$> o .: "id"
                         <*> o .: "originId"
                         <*> o .: "fingerprint"
                         <*> o .: "title"
                         <*> (posixSecondsToUTCTime . (/ 1000) . fromInteger <$> o .: "published")
                         <*> (posixSecondsToUTCTime . (/ 1000) . fromInteger <$> o .: "crawled")
                         <*> (alternateList . FeedAlternateList <$> o .: "alternate")
                         <*> o .: "origin"
                         <*> o .: "summary"
                         <*> (o .:? "visual")
                         <*> o .: "unread"
                         <*> (categoryList . FeedCategoryList <$> o .: "categories")
                         <*> o .:? "engagement" .!= 0
                         <*> o .:? "engagementRate" .!= 0

data FeedList = FeedList { items :: [FeedItem]} deriving (Show)

instance FromJSON FeedList where
  parseJSON = withObject "Feed List" $ \o ->
                FeedList <$> o .: "items"

feedList :: FilePath -> IO (Either String FeedList)
feedList p = eitherDecode <$> B.readFile p
