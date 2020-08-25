{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleInstances #-}
module Werewolf.V3.SeihekiDao.Class
    ( MonadSeihekiDaoReadOnly (..)
    , MonadSeihekiDao (..)
    ) where

import           Control.Exception.Safe    (MonadThrow, catch, throw)
import           Control.Monad.Cont        (ContT (..))
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class
import           Data.Acid.Abstract        (AcidState, query', update')
import qualified Data.Map.Strict           as M
import qualified Data.Set                  as S
import           Servant.Server            (err404)
import           Werewolf.V3.Seiheki
import qualified Werewolf.V3.SimpleDB      as DB
import           Werewolf.Werewolf         (Werewolf)


class MonadSeihekiDaoReadOnly m where
    lookupSeiheki :: SeihekiId -> m Seiheki
    getSeihekis :: (Seiheki -> Bool) -> m SeihekiMap
    getSeihekisRestrictedBy :: S.Set SeihekiId -> m SeihekiMap

class MonadSeihekiDaoReadOnly m => MonadSeihekiDao m where
    postSeiheki :: Seiheki -> m SeihekiId
    patchSeiheki :: (Seiheki -> Seiheki) -> SeihekiId -> m ()

lookupSeiheki' :: (MonadIO m, MonadThrow m) => AcidState (DB.SimpleDB Seiheki) -> SeihekiId -> m Seiheki
lookupSeiheki' seihekiDB seihekiId = query' seihekiDB (DB.GetValue seihekiId) >>= throw err404 `maybe` return

dbIdentifier :: String
dbIdentifier = "v3/seiheki"

instance MonadSeihekiDaoReadOnly Werewolf where
    lookupSeiheki seihekiId = flip runContT return do
        seihekiDB <- ContT $ DB.withSimpleDB dbIdentifier
        lookupSeiheki' seihekiDB seihekiId
        `catch` throwError
    getSeihekis f = flip runContT return $ do
        seihekiDB <- ContT $ DB.withSimpleDB dbIdentifier
        M.filter f <$> query' seihekiDB DB.GetValues
    getSeihekisRestrictedBy keys = flip runContT return $ do
        seihekiDB <- ContT $ DB.withSimpleDB dbIdentifier
        flip M.restrictKeys keys <$> query' seihekiDB DB.GetValues

instance MonadSeihekiDao Werewolf where
    postSeiheki seiheki = flip runContT return $ do
        seihekiDB <- ContT $ DB.withSimpleDB dbIdentifier
        update' seihekiDB (DB.PostValue seiheki)
    patchSeiheki f seihekiId = flip runContT return do
        seihekiDB <- ContT $ DB.withSimpleDB dbIdentifier
        seiheki <- lookupSeiheki' seihekiDB seihekiId
        update' seihekiDB . DB.PutValue seihekiId $ f seiheki
        `catch` throwError
