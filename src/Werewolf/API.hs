{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Werewolf.API where

import           Data.Proxy             (Proxy (..))
import           Servant.API
import qualified Werewolf.V2.Server.API as V2
import qualified Werewolf.V3.Server.API as V3


type API = V2.API :<|> V3.API

api :: Proxy API
api = Proxy
