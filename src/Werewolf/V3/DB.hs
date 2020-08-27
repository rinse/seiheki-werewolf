{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Werewolf.V3.DB where

import           Control.Monad.Reader.Class
import           Control.Monad.State.Class
import           Data.Acid
import qualified Data.Map                   as M
import           Data.SafeCopy
import           Werewolf.V3.Deck
import           Werewolf.V3.History
import           Werewolf.V3.Seiheki
import           Werewolf.V3.SeihekiComment


data DB = DB
    { seihekiDB        :: M.Map SeihekiId Seiheki
    , seihekiCommentDB :: M.Map SeihekiCommentId SeihekiComment
    , deckDB           :: Deck
    , historyDB        :: History
    }

emptyDB :: DB
emptyDB = DB M.empty M.empty (Deck []) (History [])

$(deriveSafeCopy 0 'base ''DB)

lookupSeiheki :: SeihekiId -> Query DB (Maybe Seiheki)
lookupSeiheki key = M.lookup key . seihekiDB <$> ask

insertSeiheki :: SeihekiId -> Seiheki -> Update DB ()
insertSeiheki key item = modify' $ \db@DB{..} -> db { seihekiDB = M.insert key item seihekiDB }

postSeiheki :: Seiheki -> Update DB SeihekiId
postSeiheki item = do
    maxId <- fmap fst . M.lookupMax . seihekiDB <$> get
    let newId = maybe (toEnum 0) succ maxId
    insertSeiheki newId item
    return newId

getSeihekis :: Query DB (M.Map SeihekiId Seiheki)
getSeihekis = seihekiDB <$> ask

lookupSeihekiComment :: SeihekiCommentId -> Query DB (Maybe SeihekiComment)
lookupSeihekiComment key = M.lookup key . seihekiCommentDB <$> ask

insertSeihekiComment :: SeihekiCommentId -> SeihekiComment -> Update DB ()
insertSeihekiComment key item = modify' $ \db@DB{..} -> db { seihekiCommentDB = M.insert key item seihekiCommentDB }

postSeihekiComment :: SeihekiComment -> Update DB SeihekiCommentId
postSeihekiComment item = do
    maxId <- fmap fst . M.lookupMax . seihekiCommentDB <$> get
    let newId = maybe (toEnum 0) succ maxId
    insertSeihekiComment newId item
    return newId

getSeihekiComments :: Query DB (M.Map SeihekiCommentId SeihekiComment)
getSeihekiComments = seihekiCommentDB <$> ask

getDeck :: Query DB Deck
getDeck = deckDB <$> ask

putDeck :: Deck -> Update DB ()
putDeck deck = do
    db@DB{..} <- get
    put $ db {deckDB = deck}

$(makeAcidic ''DB
    [ 'lookupSeiheki
    , 'insertSeiheki
    , 'postSeiheki
    , 'getSeihekis
    , 'lookupSeihekiComment
    , 'insertSeihekiComment
    , 'postSeihekiComment
    , 'getSeihekiComments
    , 'getDeck
    , 'putDeck
    ])
