{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Werewolf.V1.Server.Handler where

import           Control.Exception.Safe    (MonadCatch, MonadThrow, catchAny,
                                            throw)
import           Control.Monad
import           Control.Monad.Cont
import           Control.Monad.State.Class (get, put)
import           Data.Aeson.Text           (encodeToLazyText)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import qualified Data.Text.Lazy            as TL
import           Servant.API
import           Servant.Server
import           System.Random.Shuffle     (shuffleM)
import           Werewolf.ThemeInfo
import           Werewolf.V1.Server.API
import           Werewolf.Werewolf


handler :: ServerT API Werewolf
handler = appendTheme
    :<|> getCurrentTheme
    :<|> getAnswer
    :<|> shuffleTheme
    :<|> nextTheme
    :<|> showAll
    :<|> loadTheme
    :<|> dispose

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

loadBackup :: (MonadIO m, MonadCatch m) => m [ThemeInfo]
loadBackup = (read <$> liftIO (readFile "backup.txt")) `catchAny` const (return [])
