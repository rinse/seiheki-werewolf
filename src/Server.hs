{-# LANGUAGE OverloadedStrings #-}
module Server where

import           Control.Concurrent.STM     (TVar, atomically, newTVar)
import           Control.Monad.Cont
import qualified Data.ByteString.Lazy       as BL
import qualified Network.Wai.Handler.Warp   as Warp
import           Servant.API
import           Servant.Server
import           Werewolf.API
import           Werewolf.ThemeInfo
import qualified Werewolf.V1.Server.Handler as V1
import qualified Werewolf.V2.Server.Handler as V2
import qualified Werewolf.V3.Server.Handler as V3
import           Werewolf.Werewolf


showIndex :: Werewolf BL.ByteString
showIndex = liftIO $ BL.readFile "static/index.html"

server :: ServerT API Werewolf
server = showIndex
    :<|> V1.handler
    :<|> V2.handler
    :<|> V3.handler

hoistWerewolf :: TVar [ThemeInfo] -> ServerT API Werewolf -> Server API
hoistWerewolf s = hoistServer api $ fmap fst . flip runWerewolf s

runServer :: IO ()
runServer = do
    putStrLn "Listening on port 8080"
    themeInfo <- V1.loadBackup
    s <- atomically $ newTVar themeInfo
    Warp.run 8080 . serve api $ hoistWerewolf s server
