{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Werewolf.V3.DB.Class where

import           Control.Monad.Reader.Class
import           Data.Acid.Abstract         (AcidState)
import           Werewolf.V3.DB
import           Werewolf.Werewolf          (Werewolf (..))


class MonadDB m where
    getAcidState :: m (AcidState DB)

instance MonadDB Werewolf where
    getAcidState = Werewolf ask
