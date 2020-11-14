{-# LANGUAGE DeriveGeneric #-}

module Werewolf.V3.History where

import           Control.DeepSeq        (NFData)
import           Data.Aeson             (FromJSON, ToJSON, genericParseJSON,
                                         genericToJSON, parseJSON, toJSON)
import           Data.Aeson.Casing      (aesonPrefix, camelCase)
import           Data.SafeCopy.Internal (SafeCopy)
import           GHC.Generics
import           Web.FormUrlEncoded     (FromForm)
import           Werewolf.V3.Seiheki    (SeihekiId)

newtype History = History
    { unHistory :: [SeihekiId]
    } deriving (Read, Show, Generic)

instance NFData History
instance FromJSON History where
    parseJSON = genericParseJSON $ aesonPrefix camelCase
instance ToJSON History where
    toJSON = genericToJSON $ aesonPrefix camelCase
instance FromForm History
instance SafeCopy History
