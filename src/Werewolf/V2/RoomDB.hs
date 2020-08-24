{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Werewolf.V2.RoomDB
    ( RoomDB (..)
    , emptyDB
    , withRoomDB
    , viewThemeInfo
    , addThemeInfo
    , putThemeInfo
    , viewHistory
    , addHistory
    ) where

import           Control.DeepSeq            (NFData)
import           Control.Exception.Safe
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Control.Monad.State.Class
import           Data.Acid
import qualified Data.Map.Strict            as M
import           Data.SafeCopy
import qualified Data.Text                  as T
import           Data.UUID                  (UUID, toText)
import           GHC.Generics               (Generic)
import           Werewolf.ThemeInfo


type Deck = [ThemeInfo]
type History = [ThemeInfo]
data Item = Item
    { deck    :: Deck
    , history :: History
    } deriving (Generic)

instance NFData Item
instance SafeCopy Item

newtype RoomDB = RoomDB
    { unRoomDB :: M.Map T.Text Item
    }

emptyDB :: RoomDB
emptyDB = RoomDB M.empty

mapRoomDB :: (M.Map T.Text Item -> M.Map T.Text Item) -> RoomDB -> RoomDB
mapRoomDB f (RoomDB a) = RoomDB $ f a

$(deriveSafeCopy 0 'base ''RoomDB)

getItem :: T.Text -> Query RoomDB (Maybe Item)
getItem key =
    ask >>= (return . M.lookup key) . unRoomDB

putItem :: T.Text -> Item -> Update RoomDB ()
putItem key item =
    modify' . mapRoomDB $ M.insert key item

$(makeAcidic ''RoomDB ['getItem, 'putItem])

viewItem :: MonadIO m => AcidState RoomDB -> UUID -> m (Maybe Item)
viewItem roomDB roomId = liftIO $ query roomDB (GetItem $ toText roomId)

setItem :: MonadIO m => AcidState RoomDB -> UUID -> Item -> m ()
setItem roomDB roomId item = liftIO . update roomDB $ PutItem (toText roomId) item

updateItem :: MonadIO m => AcidState RoomDB -> UUID -> (Maybe Item -> Item) -> m ()
updateItem roomDB roomId f = viewItem roomDB roomId >>= setItem roomDB roomId . f

viewThemeInfo :: MonadIO m => AcidState RoomDB -> UUID -> m Deck
viewThemeInfo roomDB roomId = maybe [] deck <$> viewItem roomDB roomId

viewHistory :: MonadIO m => AcidState RoomDB -> UUID -> m History
viewHistory roomDB roomId = maybe [] history <$> viewItem roomDB roomId

addThemeInfo :: MonadIO m => AcidState RoomDB -> UUID -> [ThemeInfo] -> m ()
addThemeInfo roomDB roomId themeInfo = do
    updateItem roomDB roomId $ \case
        Nothing -> Item [] themeInfo
        Just (Item deck history) -> Item (deck <> themeInfo) history

putThemeInfo :: MonadIO m => AcidState RoomDB -> UUID -> [ThemeInfo] -> m ()
putThemeInfo roomDB roomId deck = do
    updateItem roomDB roomId $ \case
        Nothing -> Item deck []
        Just item' -> item' {deck = deck}

addHistory :: MonadIO m => AcidState RoomDB -> UUID -> [ThemeInfo] -> m ()
addHistory roomDB roomId themeInfo = do
    updateItem roomDB roomId $ \case
        Nothing -> Item [] themeInfo
        Just (Item deck history) -> Item deck (history <> themeInfo)

openLocalRoomDB :: MonadIO m => m (AcidState RoomDB)
openLocalRoomDB = liftIO $ openLocalStateFrom "roomDB/" emptyDB

closeRoomDB :: MonadIO m => AcidState RoomDB -> m ()
closeRoomDB = liftIO . closeAcidState

withRoomDB :: (MonadIO m, MonadMask m) => (AcidState RoomDB -> m a) -> m a
withRoomDB = bracket openLocalRoomDB closeRoomDB

