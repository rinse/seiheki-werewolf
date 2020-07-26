{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Werewolf.RoomDB
    ( RoomDB (..)
    , emptyDB
    , withRoomDB
    , PutThemeInfo (..)
    , AddThemeInfo (..)
    , ViewThemeInfo (..)
    ) where

import           Control.Exception.Safe
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Control.Monad.State.Class
import           Data.Acid
import qualified Data.Map.Strict            as M
import           Data.Maybe
import           Data.SafeCopy
import qualified Data.Text                  as T
import           Werewolf.ThemeInfo


newtype RoomDB = RoomDB
    { unRoomDB :: M.Map T.Text [ThemeInfo]
    } deriving (Read, Show)

emptyDB :: RoomDB
emptyDB = RoomDB M.empty

mapRoomDB :: (M.Map T.Text [ThemeInfo] -> M.Map T.Text [ThemeInfo]) -> RoomDB -> RoomDB
mapRoomDB f (RoomDB a) = RoomDB $ f a

$(deriveSafeCopy 0 'base ''RoomDB)

putThemeInfo :: (T.Text, [ThemeInfo]) -> Update RoomDB ()
putThemeInfo (uuid, themeInfo) =
    modify' . mapRoomDB $ M.insert uuid themeInfo

addThemeInfo :: (T.Text, [ThemeInfo]) -> Update RoomDB ()
addThemeInfo (uuid, themeInfo) =
    modify' . mapRoomDB $ M.alter (Just . maybe themeInfo (<> themeInfo)) uuid

viewThemeInfo :: T.Text -> Query RoomDB [ThemeInfo]
viewThemeInfo uuid =
    ask >>= (return . fromMaybe [] . M.lookup uuid) . unRoomDB

$(makeAcidic ''RoomDB ['putThemeInfo, 'addThemeInfo, 'viewThemeInfo])

openLocalRoomDB :: MonadIO m => m (AcidState RoomDB)
openLocalRoomDB = liftIO $ openLocalStateFrom "roomDB/" emptyDB

closeRoomDB :: MonadIO m => AcidState RoomDB -> m ()
closeRoomDB = liftIO . closeAcidState

withRoomDB :: (MonadIO m, MonadMask m) => (AcidState RoomDB -> m a) -> m a
withRoomDB = bracket openLocalRoomDB closeRoomDB
