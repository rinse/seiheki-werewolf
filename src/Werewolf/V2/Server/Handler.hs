{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Werewolf.V2.Server.Handler where

import           Control.DeepSeq
import           Control.Exception.Safe (MonadThrow, throw)
import           Control.Monad.Cont
import qualified Data.ByteString.Lazy   as BL
import           Data.UUID              (UUID)
import           Data.UUID.V4           (nextRandom)
import           Servant.API
import           Servant.Server
import           Werewolf.ThemeInfo
import           Werewolf.Utils
import           Werewolf.RoomDB
import           Werewolf.V2.Server.API
import           Werewolf.Werewolf


handler :: ServerT API Werewolf
handler = createRoom
    :<|> showRoom
    :<|> appendThemeInfo
    :<|> getTop
    :<|> shuffleTheme'
    :<|> popTheme
    :<|> showAll'
    :<|> showHistory

createRoom :: Werewolf UUID
createRoom = do
    roomId <- liftIO nextRandom
    flip runContT return $ do
        roomDB <- ContT withRoomDB
        addThemeInfo roomDB roomId []
    return roomId

showRoom :: UUID -> Werewolf BL.ByteString
showRoom _ = liftIO $ BL.readFile "static/room.html"

appendThemeInfo :: UUID -> ThemeInfo -> Werewolf ()
appendThemeInfo roomId themeInfo =
    flip runContT return $ do
        roomDB <- ContT withRoomDB
        addThemeInfo roomDB roomId [themeInfo]

headMaybe :: [a] -> Maybe a
headMaybe []    = Nothing
headMaybe (x:_) = Just x

getTop :: UUID -> Werewolf ThemeInfo
getTop roomId = do
    flip runContT return $ do
        roomDB <- ContT withRoomDB
        themeInfo <- viewThemeInfo roomDB roomId
        ret <- maybe throwNoThemeError return $ headMaybe themeInfo
        return $ force ret

shuffleTheme' :: UUID -> Werewolf ()
shuffleTheme' roomId = flip runContT return $ do
    roomDB <- ContT withRoomDB
    viewThemeInfo roomDB roomId
        >>= liftIO . groupedShuffle name
        >>= putThemeInfo roomDB roomId

popTheme :: UUID -> Werewolf ()
popTheme roomId = flip runContT return $ do
    roomDB <- ContT withRoomDB
    themeInfo <- viewThemeInfo roomDB roomId
    case themeInfo of
        []     -> throwNoThemeError
        (x:xs) -> do
            putThemeInfo roomDB roomId xs
            addHistory roomDB roomId [x]

showAll' :: UUID -> Werewolf [ThemeInfo]
showAll' roomId = do
    flip runContT return $ do
        roomDB <- ContT withRoomDB
        themeInfo <- viewThemeInfo roomDB roomId
        return $ force themeInfo

showHistory :: UUID -> Werewolf [ThemeInfo]
showHistory roomId = do
    flip runContT return $ do
        roomDB <- ContT withRoomDB
        themeInfo <- viewHistory roomDB roomId
        themeInfo `deepseq` return themeInfo

throwNoThemeError :: MonadThrow m => m a
throwNoThemeError = throw $ err400 {errBody = "NoThemeError"}
