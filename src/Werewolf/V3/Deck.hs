{-# LANGUAGE DeriveGeneric #-}

module Werewolf.V3.Deck where

import           Control.DeepSeq        (NFData)
import           Data.Aeson             (FromJSON, ToJSON, genericParseJSON,
                                         genericToJSON, parseJSON, toJSON)
import           Data.Aeson.Casing      (aesonPrefix, camelCase)
import           Data.SafeCopy.Internal (SafeCopy)
import           GHC.Generics
import           Web.FormUrlEncoded     (FromForm)
import           Werewolf.V3.Seiheki    (SeihekiId)

newtype Deck = Deck
    { unDeck :: [SeihekiId]
    } deriving (Read, Show, Generic)

instance NFData Deck
instance FromJSON Deck where
    parseJSON = genericParseJSON $ aesonPrefix camelCase
instance ToJSON Deck where
    toJSON = genericToJSON $ aesonPrefix camelCase
instance FromForm Deck
instance SafeCopy Deck
