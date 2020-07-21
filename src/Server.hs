{-# LANGUAGE OverloadedStrings #-}
module Server where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy     as BL
import qualified Network.Wai.Handler.Warp as Warp
import           Servant.Server
import           Werewolf.API


showIndex :: Handler BL.ByteString
showIndex = liftIO $ BL.readFile "static/index.html"

server :: Server API
server = showIndex

runServer :: IO ()
runServer = do
    putStrLn "Listening on port 8080"
    Warp.run 8080 . serve api $ server
