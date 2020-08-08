{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Werewolf.API where

import qualified Data.ByteString.Lazy as BL
import           Data.Proxy           (Proxy (..))
import           Data.Text            (Text)
import           Data.UUID            (UUID)
import           Servant.API
import           Servant.HTML         (HTML)
import           Werewolf.ThemeInfo   (ThemeInfo)


type API = Get '[HTML] BL.ByteString
    :<|> "v1" :> "seiheki" :> ReqBody '[JSON, FormUrlEncoded] ThemeInfo :> Post '[JSON] ()
    :<|> "v1" :> "seiheki" :> "theme" :> Get '[JSON] Text
    :<|> "v1" :> "seiheki" :> "name" :> Get '[JSON] Text
    :<|> "v1" :> "seiheki" :> "shuffle" :> Post '[JSON] ()
    :<|> "v1" :> "seiheki" :> "next" :> Post '[JSON] ()
    :<|> "v1" :> "seiheki" :> "all" :> Get '[JSON] [ThemeInfo]
    :<|> "v1" :> "seiheki" :> "load" :> Post '[JSON] ()
    :<|> "v1" :> "seiheki" :> "dispose" :> Post '[JSON] ()
    :<|> "v2" :> "room" :> "create" :> Post '[JSON] UUID
    :<|> "v2" :> "room" :> Capture "id" UUID :> Get '[HTML] BL.ByteString
    :<|> "v2" :> "room" :> Capture "id" UUID :> "theme-info" :> ReqBody '[JSON, FormUrlEncoded] ThemeInfo :> Post '[JSON] ()
    :<|> "v2" :> "room" :> Capture "id" UUID :> "theme-info" :> "top" :> Get '[JSON] ThemeInfo
    :<|> "v2" :> "room" :> Capture "id" UUID :> "shuffle" :> Post '[JSON] ()
    :<|> "v2" :> "room" :> Capture "id" UUID :> "next" :> Post '[JSON] ()
    :<|> "v2" :> "room" :> Capture "id" UUID :> "all" :> Get '[JSON] [ThemeInfo]
    :<|> "v2" :> "room" :> Capture "id" UUID :> "history" :> Get '[JSON] [ThemeInfo]

api :: Proxy API
api = Proxy
