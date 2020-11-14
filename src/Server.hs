{-# LANGUAGE OverloadedStrings #-}
module Server where

import           Control.Concurrent.STM     (TVar, atomically, newTVar)
import           Control.Exception.Safe     (bracket)
import           Control.Monad.Cont
import           Data.Acid
import qualified Data.ByteString.Lazy       as BL
import qualified Network.Wai.Handler.Warp   as Warp
import           Servant.API
import           Servant.Server
import           Werewolf.API
import           Werewolf.ThemeInfo
import qualified Werewolf.V1.Server.Handler as V1
import qualified Werewolf.V2.Server.Handler as V2
import           Werewolf.V3.DB             (DB (..), emptyDB)
import qualified Werewolf.V3.Server.Handler as V3
import           Werewolf.Werewolf


showIndex :: Werewolf BL.ByteString
showIndex = liftIO $ BL.readFile "static/index.html"

server :: ServerT API Werewolf
server = showIndex
    :<|> V1.handler
    :<|> V2.handler
    :<|> V3.handler

hoistWerewolf :: AcidState DB -> TVar [ThemeInfo] -> ServerT API Werewolf -> Server API
hoistWerewolf a t s = do
    hoistServer api f s
    where
    f :: Werewolf a -> Handler a
    f w = fst <$> runWerewolf w a t

runServer :: IO ()
runServer = do
    putStrLn "Listening on port 8080"
    themeInfo <- V1.loadBackup
    tv <- atomically $ newTVar themeInfo
    flip runContT return $ do
        st' <- ContT $ bracket (openLocalStateFrom "db/v3" emptyDB) closeAcidState
        liftIO . Warp.run 8080 . serve api $ hoistWerewolf st' tv server
