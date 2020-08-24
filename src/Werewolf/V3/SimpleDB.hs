{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TemplateHaskell #-}
module Werewolf.V3.SimpleDB where

import           Control.Exception.Safe
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Control.Monad.State.Class
import           Data.Acid
import qualified Data.Map.Strict            as M
import           Data.SafeCopy


-- |Primary key of SimpleDB.
type PrimaryKey = Int

-- |Simple DataBase which has affinity to REST API.
newtype SimpleDB value = SimpleDB
    { unSimpleDB :: M.Map PrimaryKey value
    }

$(deriveSafeCopy 0 'base ''SimpleDB)

-- |Gets a value on DB.
getValue :: PrimaryKey -> Query (SimpleDB v) (Maybe v)
getValue key = M.lookup key . unSimpleDB <$> ask

-- |Puts a value on DB.
putValue :: PrimaryKey -> v -> Update (SimpleDB v) ()
putValue key item = modify' . mapSimpleDB $ M.insert key item

mapSimpleDB :: (M.Map PrimaryKey v -> M.Map PrimaryKey v) -> SimpleDB v -> SimpleDB v
mapSimpleDB f = SimpleDB . f . unSimpleDB

-- |Posts a value to DB.
postValue :: v -> Update (SimpleDB v) PrimaryKey
postValue item = do
    maxId <- fmap fst . M.lookupMax . unSimpleDB <$> get
    let newId = maybe (toEnum 0) succ maxId
    putValue newId item
    return newId

-- |Gets values on DB.
getValues :: Query (SimpleDB v) (M.Map PrimaryKey v)
getValues = unSimpleDB <$> ask

$(makeAcidic ''SimpleDB ['getValue, 'putValue, 'postValue, 'getValues])

-- |Obtains a context using DB.
withSimpleDB :: (MonadIO m, MonadMask m, SafeCopy v, Typeable v)
             => String -> (AcidState (SimpleDB v) -> m a) -> m a
withSimpleDB s = bracket (liftIO . openLocalStateFrom s $ SimpleDB M.empty) (liftIO . closeAcidState)
