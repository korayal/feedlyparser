{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Combo
      ( ComboState(..)
      , runCombo
      , mkComboState
      , Combo(..)) where

import           Control.Concurrent.STM
import           Control.Exception      as E
import           Control.Monad.Reader
import qualified Data.Aeson             as A
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL
import           Types

data ComboState = ComboState
    { _fl :: !(TVar FeedList)
    , _nl :: !(TVar [Integer])
    }

newtype Combo m a = Combo
    { unCombo :: ReaderT ComboState m a
    } deriving (Functor,Applicative,Monad,MonadReader ComboState,MonadIO)

runCombo :: ComboState -> Combo m a  -> m a
runCombo cs f = runReaderT (unCombo f) cs

mkComboState :: FilePath -> IO ComboState
mkComboState p = do
    f <- BL.readFile p
    case A.eitherDecode f of
        Left _ -> error "input file is not a valid json"
        Right fl ->
            ComboState <$> (newTVarIO fl) <*> (newTVarIO ([] :: [Integer]))
