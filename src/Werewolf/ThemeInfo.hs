{-# LANGUAGE DeriveGeneric #-}
module Werewolf.ThemeInfo where

import           Control.DeepSeq    (NFData)
import           Data.Aeson         (FromJSON, ToJSON)
import           Data.SafeCopy      (SafeCopy)
import           Data.Text          (Text)
import           GHC.Generics       (Generic)
import           Web.FormUrlEncoded (FromForm)


data ThemeInfo = ThemeInfo
    { name  :: Text
    , theme :: Text
    } deriving (Read, Show, Generic)

instance NFData ThemeInfo
instance FromJSON ThemeInfo
instance ToJSON ThemeInfo
instance FromForm ThemeInfo
instance SafeCopy ThemeInfo
