{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleInstances #-}
module Werewolf.V3.DeckDao.Class
    ( MonadDeckDaoReadOnly (..)
    , MonadDeckDao (..)
    ) where

import           Control.Monad.Cont    (ContT (..))
import           Data.Acid.Abstract    (query', update')
import           Werewolf.V3.Deck      (Deck (..))
import qualified Werewolf.V3.TrivialDB as DB
import           Werewolf.Werewolf     (Werewolf)


class MonadDeckDaoReadOnly m where
    getDeck  :: m Deck

class MonadDeckDaoReadOnly m => MonadDeckDao m where
    modifyDeck :: (Deck -> Deck) -> m ()
    putDeck :: Deck -> m ()

dbIdentifier :: String
dbIdentifier = "deck"

instance MonadDeckDaoReadOnly Werewolf where
    getDeck = flip runContT return $ do
        db <- ContT $ DB.withTrivialDB dbIdentifier (Deck [])
        query' db DB.GetValue

instance MonadDeckDao Werewolf where
    modifyDeck f = flip runContT return $ do
        db <- ContT $ DB.withTrivialDB dbIdentifier (Deck [])
        v <- query' db DB.GetValue
        update' db (DB.PutValue (f v))
    putDeck deck = flip runContT return $ do
        db <- ContT $ DB.withTrivialDB dbIdentifier (Deck [])
        update' db (DB.PutValue deck)
