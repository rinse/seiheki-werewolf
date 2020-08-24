{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Server where

import           Control.Concurrent.STM    (TVar, atomically, newTVar)
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
import qualified Network.Wai.Handler.Warp  as Warp
import           Servant.API
import           Servant.Server
import           System.Random.Shuffle     (shuffleM)
import           Werewolf.API
import           Werewolf.ThemeInfo
import           Werewolf.Werewolf
import qualified Werewolf.V2.Server.Handler as V2
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
    :<|> V2.handler
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
