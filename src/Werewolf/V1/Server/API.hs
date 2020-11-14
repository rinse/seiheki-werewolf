{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Werewolf.V1.Server.API where

import           Data.Text          (Text)
import           Servant.API
import           Werewolf.ThemeInfo (ThemeInfo)


type API = "v1" :> "seiheki" :> ReqBody '[JSON, FormUrlEncoded] ThemeInfo :> Post '[JSON] ()
    :<|> "v1" :> "seiheki" :> "theme" :> Get '[JSON] Text
    :<|> "v1" :> "seiheki" :> "name" :> Get '[JSON] Text
    :<|> "v1" :> "seiheki" :> "shuffle" :> Post '[JSON] ()
    :<|> "v1" :> "seiheki" :> "next" :> Post '[JSON] ()
    :<|> "v1" :> "seiheki" :> "all" :> Get '[JSON] [ThemeInfo]
    :<|> "v1" :> "seiheki" :> "load" :> Post '[JSON] ()
    :<|> "v1" :> "seiheki" :> "dispose" :> Post '[JSON] ()