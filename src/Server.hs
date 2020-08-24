{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Server where

import           Control.Concurrent.STM    (TVar, atomically, newTVar)
import           Control.DeepSeq
import           Control.Exception.Safe    (MonadCatch, MonadThrow, catchAny,
                                            throw)
import           Control.Monad
import           Control.Monad.Cont
import           Control.Monad.State.Class (get, put)
import           Data.Aeson.Text           (encodeToLazyText)
import qualified Data.ByteString.Lazy      as BL
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import qualified Data.Text.Lazy            as TL
import           Data.UUID                 (UUID)
import           Data.UUID.V4              (nextRandom)
import qualified Network.Wai.Handler.Warp  as Warp
import           Servant.API
import           Servant.Server
import           System.Random.Shuffle     (shuffleM)
import           Werewolf.API
import           Werewolf.RoomDB
import           Werewolf.ThemeInfo
import           Werewolf.Utils
import           Werewolf.Werewolf
import qualified Werewolf.V3.Server.Handler as V3


showIndex :: Werewolf BL.ByteString
showIndex = liftIO $ BL.readFile "static/index.html"

appendTheme :: ThemeInfo -> Werewolf ()
appendTheme themeInfo = unless (T.null $ theme themeInfo) $ do
    s <- (++ [themeInfo]) <$> get
    put s
    saveBackup s

throwNoThemeError :: MonadThrow m => m a
throwNoThemeError = throw $ err400 {errBody = "NoThemeError"}

getCurrentTheme :: Werewolf Text
getCurrentTheme = get >>= \case
    [] -> throwNoThemeError
    (x:_) -> return (theme x)

getAnswer :: Werewolf Text
getAnswer = get >>= \case
    [] -> throwNoThemeError
    (x:_) -> return $ name x

shuffleTheme :: Werewolf ()
shuffleTheme = get >>= liftIO . shuffleM >>= put

nextTheme :: Werewolf ()
nextTheme = do
    get >>= \case
        [] -> throwNoThemeError
        (x:xs) -> appendHistory x >> put xs >> saveBackup xs
    where
    appendHistory :: MonadIO m => ThemeInfo -> m ()
    appendHistory = liftIO . T.appendFile "history.log" . (<> "\n") . TL.toStrict . encodeToLazyText

saveBackup :: MonadIO m => [ThemeInfo] -> m ()
saveBackup = liftIO . writeFile "backup.txt" . show

showAll :: Werewolf [ThemeInfo]
showAll = get

loadTheme :: Werewolf ()
loadTheme = loadBackup >>= put

dispose :: Werewolf ()
dispose = put []



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

server :: ServerT API Werewolf
server = showIndex
    :<|> appendTheme
    :<|> getCurrentTheme
    :<|> getAnswer
    :<|> shuffleTheme
    :<|> nextTheme
    :<|> showAll
    :<|> loadTheme
    :<|> dispose
    :<|> createRoom
    :<|> showRoom
    :<|> appendThemeInfo
    :<|> getTop
    :<|> shuffleTheme'
    :<|> popTheme
    :<|> showAll'
    :<|> showHistory
    :<|> V3.handler

hoistWerewolf :: TVar [ThemeInfo] -> ServerT API Werewolf -> Server API
hoistWerewolf s = hoistServer api $ fmap fst . flip runWerewolf s

loadBackup :: (MonadIO m, MonadCatch m) => m [ThemeInfo]
loadBackup = (read <$> liftIO (readFile "backup.txt")) `catchAny` const (return [])

runServer :: IO ()
runServer = do
    putStrLn "Listening on port 8080"
    themeInfo <- loadBackup
    s <- atomically $ newTVar themeInfo
    Warp.run 8080 . serve api $ hoistWerewolf s server
