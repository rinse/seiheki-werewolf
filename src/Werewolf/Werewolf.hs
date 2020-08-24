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
import           Servant.Server             (Handler (..), ServerError)
import           Werewolf.ThemeInfo         (ThemeInfo)


newtype Werewolf a = Werewolf
    { unWerewolf :: AtomicStateT [ThemeInfo] Handler a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

runWerewolf :: Werewolf a -> TVar [ThemeInfo] -> Handler (a, [ThemeInfo])
runWerewolf = runAtomicStateT . unWerewolf

instance MonadState [ThemeInfo] Werewolf where
    put = Werewolf . put
    get = Werewolf get

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
liftCatch base (Werewolf (AtomicStateT a)) handler = Werewolf . AtomicStateT $ R.liftCatch base a (ReaderT . handler')
    where
    handler' serverError = fmap fst . runAtomicStateT (unWerewolf $ handler serverError)
