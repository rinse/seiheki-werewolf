{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleInstances #-}
module Werewolf.V3.SeihekiCommentDao.Class
    ( MonadSeihekiCommentDaoReadOnly (..)
    , MonadSeihekiCommentDao (..)
    ) where

import           Control.Exception.Safe     (MonadThrow, catch, throw)
import           Control.Monad.Cont         (ContT (..))
import           Control.Monad.Error.Class  (throwError)
import           Control.Monad.IO.Class
import           Data.Acid.Abstract         (AcidState, query', update')
import           Servant.Server             (err404)
import           Werewolf.V3.SeihekiComment
import qualified Werewolf.V3.SimpleDB       as DB
import           Werewolf.Werewolf          (Werewolf)


class MonadSeihekiCommentDaoReadOnly m where
    lookupSeihekiComment :: SeihekiCommentId -> m SeihekiComment
    getSeihekiComments :: m SeihekiCommentMap

class MonadSeihekiCommentDaoReadOnly m => MonadSeihekiCommentDao m where
    postSeihekiComment :: SeihekiComment -> m SeihekiCommentId
    patchSeihekiComment :: (SeihekiComment -> SeihekiComment) -> SeihekiCommentId -> m ()

lookupSeihekiComment' :: (MonadIO m, MonadThrow m) => AcidState (DB.SimpleDB SeihekiComment) -> SeihekiCommentId -> m SeihekiComment
lookupSeihekiComment' seihekiDB seihekiId = query' seihekiDB (DB.GetValue seihekiId) >>= throw err404 `maybe` return

dbIdentifier :: String
dbIdentifier = "v3/seiheki-comment"

instance MonadSeihekiCommentDaoReadOnly Werewolf where
    lookupSeihekiComment seihekiId = flip runContT return do
        db <- ContT $ DB.withSimpleDB dbIdentifier
        lookupSeihekiComment' db seihekiId
        `catch` throwError  -- rethrow ServerError
    getSeihekiComments = flip runContT return $ do
        db <- ContT $ DB.withSimpleDB dbIdentifier
        query' db DB.GetValues

instance MonadSeihekiCommentDao Werewolf where
    postSeihekiComment comment = flip runContT return $ do
        db <- ContT $ DB.withSimpleDB dbIdentifier
        update' db (DB.PostValue comment)
    patchSeihekiComment f commentId = flip runContT return do
        db <- ContT $ DB.withSimpleDB dbIdentifier
        comment <- lookupSeihekiComment' db commentId
        update' db . DB.PutValue commentId $ f comment
        `catch` throwError  -- rethrow ServerError
