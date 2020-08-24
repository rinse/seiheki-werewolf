{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Werewolf.V3.TrivialDB where

import           Control.Exception.Safe     (MonadMask, bracket)
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class (ask)
import           Control.Monad.State.Class  (put)
import           Data.Acid                  (AcidState, Query, Update,
                                             closeAcidState, makeAcidic,
                                             openLocalStateFrom)
import           Data.SafeCopy              (SafeCopy, base, deriveSafeCopy)
import           Data.Typeable              (Typeable)


-- |Simple DataBase which has affinity to REST API.
newtype TrivialDB a = TrivialDB
    { unTrivialDB :: a
    } deriving (Functor)

$(deriveSafeCopy 0 'base ''TrivialDB)

-- |Gets a value on DB.
getValue :: Query (TrivialDB a) a
getValue = unTrivialDB <$> ask

-- |Puts a value on DB.
putValue :: a -> Update (TrivialDB a) ()
putValue = put . TrivialDB

$(makeAcidic ''TrivialDB ['getValue, 'putValue])

-- |Obtains a context using DB.
withTrivialDB :: (MonadIO m, MonadMask m, SafeCopy v, Typeable v)
              => String -> v -> (AcidState (TrivialDB v) -> m a) -> m a
withTrivialDB dbIdentifier initialValue =
    bracket (liftIO . openLocalStateFrom dbIdentifier $ TrivialDB initialValue) (liftIO . closeAcidState)
