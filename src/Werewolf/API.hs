{-# LANGUAGE DataKinds             #-}
module Werewolf.API where

import qualified Data.ByteString.Lazy as BL
import           Data.Proxy           (Proxy (..))
import           Servant.API
import           Servant.HTML         (HTML)


type API = Get '[HTML] BL.ByteString

api :: Proxy API
api = Proxy
