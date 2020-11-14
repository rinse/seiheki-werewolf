{-# LANGUAGE OverloadedStrings #-}
module Server where

import           Control.Exception.Safe     (bracket)
import           Control.Monad.Cont
import           Data.Acid
import qualified Network.Wai.Handler.Warp   as Warp
import           Servant.Server
import           Werewolf.API
import           Werewolf.V3.DB             (DB (..), emptyDB)
import qualified Werewolf.V3.Server.Handler as V3
import           Werewolf.Werewolf


server :: ServerT API Werewolf
server = V3.handler

hoistWerewolf :: AcidState DB -> ServerT API Werewolf -> Server API
hoistWerewolf a s = do
    hoistServer api f s
    where
    f :: Werewolf a -> Handler a
    f w = runWerewolf w a

runServer :: IO ()
runServer = do
    putStrLn "Listening on port 8080"
    flip runContT return $ do
        st <- ContT $ bracket (openLocalStateFrom "db/v3" emptyDB) closeAcidState
        liftIO . Warp.run 8080 . serve api $ hoistWerewolf st server
