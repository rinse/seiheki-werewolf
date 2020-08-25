{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleInstances #-}
module Werewolf.V3.HistoryDao.Class
    ( MonadHistoryDaoReadOnly (..)
    , MonadHistoryDao (..)
    ) where

import           Control.Monad.Cont    (ContT (..))
import           Data.Acid.Abstract    (query', update')
import           Werewolf.V3.History   (History (..))
import qualified Werewolf.V3.TrivialDB as DB
import           Werewolf.Werewolf     (Werewolf)
import           Werewolf.V3.Seiheki


class MonadHistoryDaoReadOnly m where
    getHistory  :: m History

class MonadHistoryDaoReadOnly m => MonadHistoryDao m where
    addHistory :: SeihekiId -> m ()

dbIdentifier :: String
dbIdentifier = "v3/history"

instance MonadHistoryDaoReadOnly Werewolf where
    getHistory = flip runContT return $ do
        db <- ContT $ DB.withTrivialDB dbIdentifier (History [])
        query' db DB.GetValue

instance MonadHistoryDao Werewolf where
    addHistory seihekiId = flip runContT return $ do
        db <- ContT $ DB.withTrivialDB dbIdentifier (History [])
        history <- unHistory <$> query' db DB.GetValue
        update' db (DB.PutValue $ History (seihekiId:history))
