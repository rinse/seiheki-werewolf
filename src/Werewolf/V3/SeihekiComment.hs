{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Werewolf.V3.SeihekiComment where

import           Control.DeepSeq      (NFData)
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Map.Strict
import           Data.SafeCopy        (SafeCopy)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Web.FormUrlEncoded   (FromForm)


type SeihekiCommentId = Int

type SeihekiCommentMap = Map SeihekiCommentId SeihekiComment

data SeihekiComment = SeihekiComment
    { commentContent :: Text    -- ^Comment
    , commentAuthor  :: Text    -- ^Author
    , commentUpvotes :: Int     -- ^Upvotes
    } deriving (Read, Show, Generic)

instance NFData SeihekiComment
instance FromJSON SeihekiComment where
    parseJSON = genericParseJSON $ aesonPrefix camelCase
instance ToJSON SeihekiComment where
    toJSON = genericToJSON $ aesonPrefix camelCase
instance FromForm SeihekiComment
instance SafeCopy SeihekiComment
