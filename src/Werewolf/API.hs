{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Werewolf.API where

import qualified Data.ByteString.Lazy   as BL
import           Data.Proxy             (Proxy (..))
import           Servant.API
import           Servant.HTML           (HTML)
import qualified Werewolf.V1.Server.API as V1
import qualified Werewolf.V2.Server.API as V2
import qualified Werewolf.V3.Server.API as V3


type API = Get '[HTML] BL.ByteString
    :<|> V1.API
    :<|> V2.API
    :<|> V3.API

api :: Proxy API
api = Proxy
