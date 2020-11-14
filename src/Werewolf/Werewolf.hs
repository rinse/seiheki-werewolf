{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Werewolf.Werewolf where

import           Control.Concurrent.STM     (TVar)
import           Control.Exception.Safe     (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.AtomicStateT (AtomicStateT (..), runAtomicStateT)
import           Control.Monad.Error.Class  (MonadError (..))
import           Control.Monad.IO.Class
import           Control.Monad.Random.Class (MonadRandom (..))
import           Control.Monad.Reader       (ReaderT (..))
import           Control.Monad.State.Class  (MonadState, get, put)
import           Control.Monad.Trans.Class  (lift)
import qualified Control.Monad.Trans.Reader as R
import           Data.Acid                  (AcidState)
import           Servant.Server             (Handler (..), ServerError)
import           Werewolf.ThemeInfo         (ThemeInfo)
import           Werewolf.V3.DB


newtype Werewolf a = Werewolf
    { unWerewolf :: ReaderT (AcidState DB) (AtomicStateT [ThemeInfo] Handler) a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

runWerewolf :: Werewolf a -> AcidState DB -> TVar [ThemeInfo] -> Handler (a, [ThemeInfo])
runWerewolf w db = runAtomicStateT $ runReaderT (unWerewolf w)  db

instance MonadState [ThemeInfo] Werewolf where
    put = Werewolf . put
    get = Werewolf get

instance MonadError ServerError Werewolf where
    throwError = Werewolf . lift . lift . throwError
    catchError = liftCatch catchError

instance MonadRandom Werewolf where
    getRandomR = liftIO . getRandomR
    getRandom = liftIO getRandom
    getRandomRs = liftIO . getRandomRs
    getRandoms = liftIO getRandoms

liftAtomicStateCatch :: MonadIO m => (m a -> (e -> m a) -> m a)
          -> AtomicStateT s m a -> (e -> AtomicStateT s m a) -> AtomicStateT s m a
liftAtomicStateCatch base (AtomicStateT a) handler = AtomicStateT $ R.liftCatch base a (ReaderT . handler')
    where
    handler' e = fmap fst . runAtomicStateT (handler e)

liftRA :: MonadIO m => (m a -> (e -> m a) -> m a)
       -> ReaderT r (AtomicStateT s m) a -> (e -> (ReaderT r (AtomicStateT s m)) a) -> ReaderT r (AtomicStateT s m) a
liftRA base = R.liftCatch (liftAtomicStateCatch base)

liftCatch :: (Handler a -> (ServerError -> Handler a) -> Handler a)
          -> Werewolf a -> (ServerError -> Werewolf a) -> Werewolf a
liftCatch base (Werewolf r) handler = Werewolf $ liftRA base r (unWerewolf . handler)
