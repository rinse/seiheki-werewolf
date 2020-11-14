{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Werewolf.API where

import           Data.Proxy             (Proxy (..))
import qualified Werewolf.V3.Server.API as V3


type API = V3.API

api :: Proxy API
api = Proxy
