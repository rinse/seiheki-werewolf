{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Werewolf.Werewolf where

import           Control.Concurrent.STM     (TVar)
import           Control.Exception.Safe     (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.AtomicStateT (AtomicStateT, runAtomicStateT)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.State.Class  (MonadState, get, put)
import           Servant.Server             (Handler)
import           Werewolf.ThemeInfo         (ThemeInfo)


newtype Werewolf a = Werewolf
    { unWerewolf :: AtomicStateT [ThemeInfo] Handler a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

runWerewolf :: Werewolf a -> TVar [ThemeInfo] -> Handler (a, [ThemeInfo])
runWerewolf = runAtomicStateT . unWerewolf

instance MonadState [ThemeInfo] Werewolf where
    put = Werewolf . put
    get = Werewolf get
