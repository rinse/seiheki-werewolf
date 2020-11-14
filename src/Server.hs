{-# LANGUAGE OverloadedStrings #-}
module Server where

import           Control.Exception.Safe        (bracket)
import           Control.Monad.Cont
import           Data.Acid
import           Data.Foldable                 (fold)
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Middleware.Static as Wai
import           Servant.Server
import           Werewolf.API
import           Werewolf.V3.DB                (DB (..), emptyDB)
import qualified Werewolf.V3.Server.Handler    as V3
import           Werewolf.Werewolf


server :: ServerT API Werewolf
server = V3.handler

hoistWerewolf :: AcidState DB -> ServerT API Werewolf -> Server API
hoistWerewolf a s = do
    hoistServer api f s
    where
    f :: Werewolf a -> Handler a
    f w = runWerewolf w a

app :: AcidState DB -> Application
app st = middleware . serve api $ hoistWerewolf st server

{- |Replaces `x` with `y` on `s`.
    Note that this is slow af.
-}
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace x y s@(sh:st)
    | xh == x = y <> replace x y xt
    | otherwise = sh : replace x y st
    where (xh, xt) = splitAt (length x) s

policy' :: (String -> String) -> Wai.Policy
policy' f = Wai.policy $ Just . f

middleware :: Wai.Middleware
middleware = Wai.staticPolicy $ fold
    [ policy' $ replace "seiheki-werewolf-front" "./seiheki-werewolf-front/build"
    ]

runServer :: IO ()
runServer = do
    putStrLn "Listening on port 8080"
    flip runContT return $ do
        st <- ContT $ bracket (openLocalStateFrom "db/v3" emptyDB) closeAcidState
        liftIO .  Warp.run 8080 $ app st
