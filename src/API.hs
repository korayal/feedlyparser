{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module API
       (app) where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Monad.Reader
import           Servant
import           Servant.API
import qualified Servant.Server              as SS
import qualified Control.Monad.Trans.Except  as Ex
-- Local Libs
import qualified Combo                       as C
import Types
import FeedlyAPI
import NumbersAPI

type AppAPI = FeedlyAPI :<|> NumbersAPI

appServer
    :: (MonadReader C.ComboState m, MonadIO m)
    => SS.ServerT AppAPI m
appServer = feedlyServer :<|> numbersServer

appServer' :: C.ComboState -> SS.Server AppAPI
appServer' cs = SS.enter (transformStack cs) appServer

api :: Proxy AppAPI
api = Proxy

app :: C.ComboState -> Application
app cs = SS.serve api (appServer' cs)

transformStack
    :: Functor m
    => C.ComboState -> C.Combo m :~> Ex.ExceptT ServantErr m
transformStack cs = Nat (transformStack' cs)

transformStack'
    :: Functor m
    => C.ComboState -> C.Combo m a -> Ex.ExceptT ServantErr m a
transformStack' cs m = Ex.ExceptT (Right <$> C.runCombo cs m)
