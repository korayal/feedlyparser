{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
    ( feedList,
      FeedList(..),
      FeedItem(..)
    ) where

import           Control.Monad.Trans.Except
import           Data.Aeson
import qualified Data.ByteString.Lazy       as B
import           Data.Time.Clock            (UTCTime)
import           Data.Time.Clock.POSIX
import           GHC.Generics

data FeedAlternate = FeedAlternate
  { href  :: String
  , _type :: String
  } deriving (Generic, Show)

newtype FeedAlternateList = FeedAlternateList { alternateList :: [FeedAlternate] } deriving (Generic, Show)

instance ToJSON FeedAlternate
instance FromJSON FeedAlternate where
  parseJSON = withObject "Feed Alternate" $ \o ->
                FeedAlternate <$> o .: "href"
                              <*> o .: "type"

data FeedOrigin = FeedOrigin
  { streamId :: String
  , foTitle  :: String
  , htmlUrl  :: String
  } deriving (Generic, Show)

instance ToJSON FeedOrigin
instance FromJSON FeedOrigin where
  parseJSON = withObject "Feed Origin" $ \o ->
                FeedOrigin <$> o .: "streamId"
                           <*> o .: "title"
                           <*> o .: "htmlUrl"

data FeedSummary = FeedSummary
  { content   :: String
  , direction :: String
  } deriving (Generic, Show)

instance ToJSON FeedSummary
instance FromJSON FeedSummary where
  parseJSON = withObject "Feed Summary" $ \o ->
                FeedSummary <$> o .: "content"
                            <*> o .: "direction"

data FeedVisual = FeedVisual
  { url         :: String
  , width       :: Integer
  , height      :: Integer
  , processor   :: String
  , contentType :: String
  } deriving (Generic, Show)

instance ToJSON FeedVisual
instance FromJSON FeedVisual where
  parseJSON = withObject "Feed Visual" $ \o ->
                FeedVisual <$> o .: "url"
                           <*> (fromInteger <$> o .:? "width" .!= 0)
                           <*> (fromInteger <$> o .:? "height" .!= 0)
                           <*> (o .:? "processor" .!= "")
                           <*> (o .:? "contentType" .!= "")

data FeedCategory = FeedCategory
  { fcid    :: String
  , fclabel :: String
  } deriving (Generic, Show)

instance ToJSON FeedCategory
instance FromJSON FeedCategory where
  parseJSON = withObject "Feed Category" $ \o ->
                FeedCategory <$> o .: "id"
                             <*> o .: "label"

newtype FeedCategoryList = FeedCategoryList { categoryList :: [FeedCategory] } deriving (Generic, Show)


data FeedItem = FeedItem
    { id             :: String
    , originId       :: String
    , fingerprint    :: String
    , title          :: String
    , published      :: UTCTime
    , crawled        :: UTCTime
    , alternate      :: [FeedAlternate]
    , origin         :: FeedOrigin
    , summary        :: FeedSummary
    , visual         :: Maybe FeedVisual
    , unread         :: Bool
    , categories     :: [FeedCategory]
    , engagement     :: Integer
    , engagementRate :: Integer
    } deriving (Generic, Show)

instance ToJSON FeedItem
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
                         <*> o .:? "visual"
                         <*> o .: "unread"
                         <*> (categoryList . FeedCategoryList <$> o .: "categories")
                         <*> o .:? "engagement" .!= 0
                         <*> o .:? "engagementRate" .!= 0

data FeedList = FeedList { items :: [FeedItem]} deriving (Generic, Show)

instance ToJSON FeedList
instance FromJSON FeedList where
  parseJSON = withObject "Feed List" $ \o ->
                FeedList <$> o .: "items"

feedList :: FilePath -> IO (Except String FeedList)
feedList p = do
  f <- B.readFile p
  return $ (except . eitherDecode) f
