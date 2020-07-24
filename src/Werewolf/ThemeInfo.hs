{-# LANGUAGE DeriveGeneric #-}
module Werewolf.ThemeInfo where

import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Text          (Text)
import           GHC.Generics       (Generic)
import           Web.FormUrlEncoded (FromForm)


data ThemeInfo = ThemeInfo
    { name  :: Text
    , theme :: Text
    } deriving (Read, Show, Generic)

instance FromJSON ThemeInfo
instance ToJSON ThemeInfo
instance FromForm ThemeInfo
