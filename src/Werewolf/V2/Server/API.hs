{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Werewolf.V2.Server.API where

import qualified Data.ByteString.Lazy   as BL
import           Data.UUID              (UUID)
import           Servant.API
import           Servant.HTML           (HTML)
import           Werewolf.ThemeInfo     (ThemeInfo)


type API = "v2" :> "room" :> "create" :> Post '[JSON] UUID
    :<|> "v2" :> "room" :> Capture "id" UUID :> Get '[HTML] BL.ByteString
    :<|> "v2" :> "room" :> Capture "id" UUID :> "theme-info" :> ReqBody '[JSON, FormUrlEncoded] ThemeInfo :> Post '[JSON] ()
    :<|> "v2" :> "room" :> Capture "id" UUID :> "theme-info" :> "top" :> Get '[JSON] ThemeInfo
    :<|> "v2" :> "room" :> Capture "id" UUID :> "shuffle" :> Post '[JSON] ()
    :<|> "v2" :> "room" :> Capture "id" UUID :> "next" :> Post '[JSON] ()
    :<|> "v2" :> "room" :> Capture "id" UUID :> "all" :> Get '[JSON] [ThemeInfo]
    :<|> "v2" :> "room" :> Capture "id" UUID :> "history" :> Get '[JSON] [ThemeInfo]
