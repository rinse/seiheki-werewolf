{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Werewolf.V3.Seiheki where

import           Control.DeepSeq            (NFData)
import           Data.Aeson
import           Data.Aeson.Casing
import qualified Data.Map                   as M
import           Data.SafeCopy              (SafeCopy)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Web.FormUrlEncoded         (FromForm)
import           Werewolf.V3.SeihekiComment (SeihekiCommentId)


type SeihekiId = Int

type SeihekiMap = M.Map SeihekiId Seiheki

data Seiheki = Seiheki
    { seihekiContent    :: Text                 -- ^Seiheki
    , seihekiAuthor     :: Text                 -- ^Author
    , seihekiUpvotes    :: Int                  -- ^Number of upvotes
    , seihekiCommentIds :: [SeihekiCommentId]   -- ^Comments
    } deriving (Read, Show, Generic)

instance NFData Seiheki
instance FromJSON Seiheki where
    parseJSON = genericParseJSON $ aesonPrefix camelCase
instance ToJSON Seiheki where
    toJSON = genericToJSON $ aesonPrefix camelCase
instance FromForm Seiheki
instance SafeCopy Seiheki
