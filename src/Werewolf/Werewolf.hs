{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Werewolf.Werewolf where

import           Control.Exception.Safe     (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Error.Class  (MonadError (..))
import           Control.Monad.IO.Class
import           Control.Monad.Random.Class (MonadRandom (..))
import           Control.Monad.Reader       (ReaderT (..))
import           Control.Monad.Trans.Class  (lift)
import qualified Control.Monad.Trans.Reader as R
import           Data.Acid                  (AcidState)
import           Servant.Server             (Handler (..), ServerError)
import           Werewolf.V3.DB


newtype Werewolf a = Werewolf
    { unWerewolf :: ReaderT (AcidState DB) Handler a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

runWerewolf :: Werewolf a -> AcidState DB -> Handler a
runWerewolf = runReaderT . unWerewolf

instance MonadError ServerError Werewolf where
    throwError = Werewolf . lift . throwError
    catchError = liftCatch catchError

instance MonadRandom Werewolf where
    getRandomR = liftIO . getRandomR
    getRandom = liftIO getRandom
    getRandomRs = liftIO . getRandomRs
    getRandoms = liftIO getRandoms

liftCatch :: (Handler a -> (ServerError -> Handler a) -> Handler a)
          -> Werewolf a -> (ServerError -> Werewolf a) -> Werewolf a
liftCatch base (Werewolf r) handler = Werewolf $ R.liftCatch base r (unWerewolf . handler)
