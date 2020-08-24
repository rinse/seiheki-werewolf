{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}
module Werewolf.V3.Server.API where

import           Control.DeepSeq            (NFData)
import           Data.Aeson
import           Data.Aeson.Casing
import qualified Data.Map.Strict            as M
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Servant.API
import           Web.FormUrlEncoded         (FromForm)
import           Werewolf.V3.Seiheki
import           Werewolf.V3.SeihekiComment


type API = PostSeihekis :<|> GetSeihekis
    :<|> GetSeiheki
    :<|> PostSeihekiComments :<|> GetSeihekiComments
    :<|> GetSeihekiComment
    :<|> PatchSeihekiUpvote
    :<|> PostCards :<|> GetCards
    :<|> GetCard
    :<|> GetHistories

-- |Posts a seiheki and retrieve all seihekis
type PostSeihekis = "v3"
    :> "seihekis"
    :> ReqBody '[JSON, FormUrlEncoded] Seiheki
    :> PostCreated '[JSON] (Headers '[Header "Location" String] (Res201 SeihekiId))
-- |Retrieves seihekis
type GetSeihekis  = "v3"
    :> "seihekis"
    :> QueryParam "author" Text
    :> QueryParam "offset" Int
    :> QueryParam "limit" Int
    :> Get '[JSON] (ResGetCollection SeihekiId SeihekiMap)

-- |Retrieves a seiheki
type GetSeiheki = "v3"
    :> "seihekis" :> Capture "id" SeihekiId
    :> Get '[JSON] Seiheki

-- |Posts a comment and retrieve all comments on a seiheki
type PostSeihekiComments = "v3"
    :> "seihekis" :> Capture "id" SeihekiId
    :> "comments"
    :> ReqBody '[JSON, FormUrlEncoded] SeihekiComment
    :> PostCreated '[JSON] (Headers '[Header "Location" String] (Res201 SeihekiCommentId))
-- |Retrieves seiheki comments
type GetSeihekiComments = "v3"
    :> "seihekis" :> Capture "id" SeihekiId
    :> "comments"
    :> QueryParam "offset" Int
    :> QueryParam "limit" Int
    :> Get '[JSON] (ResGetCollection SeihekiCommentId SeihekiCommentMap)

-- |Retrieves a comment on a seiheki
type GetSeihekiComment = "v3"
    :> "seihekis" :> Capture "seihekiId" SeihekiId
    :> "comments" :> Capture "seihekiCommentId" SeihekiCommentId
    :> Get '[JSON] SeihekiComment

-- |Gives an upvote to a seiheki
type PatchSeihekiUpvote = "v3"
    :> "seihekis" :> Capture "seihekiId" SeihekiId
    :> "upvotes"
    :> ReqBody '[JSON, FormUrlEncoded] PatchRequest
    :> PatchNoContent '[JSON] NoContent

-- |Makes a deck from seihekis
type PostCards = "v3"
    :> "cards"
    :> PostCreated '[JSON] NoContent
-- |Retrieves a deck
type GetCards = "v3"
    :> "cards"
    :> QueryParam "offset" Int
    :> QueryParam "limit" Int
    :> Get '[JSON] (ResGetCollection SeihekiId SeihekiMap)

-- |Retrieves a seiheki on a deck
type GetCard = "v3"
    :> "cards" :> Capture "n" Int
    :> Get '[JSON] Seiheki

-- |Retrieves consumed seihekis
type GetHistories = "v3"
    :> "histories"
    :> QueryParam "offset" Int
    :> QueryParam "limit" Int
    :> Get '[JSON] (ResGetCollection SeihekiId SeihekiMap)

-- |Response entity for Post
data Res201 a = Res201
    { res201Phrase :: Text
    , res201Id     :: a
    } deriving (Read, Show, Generic)

res201 :: a -> Res201 a
res201 = Res201 "Created"

instance NFData a => NFData (Res201 a)
instance FromJSON a => FromJSON (Res201 a) where
    parseJSON = genericParseJSON $ aesonPrefix camelCase
instance ToJSON a => ToJSON (Res201 a) where
    toJSON = genericToJSON $ aesonPrefix camelCase

-- |Request entity for Patch
newtype PatchRequest = PatchRequest
    { patchOp :: Text
    } deriving (Read, Show, Generic)

instance NFData PatchRequest
instance FromJSON PatchRequest where
    parseJSON = genericParseJSON $ aesonPrefix camelCase
instance ToJSON PatchRequest where
    toJSON = genericToJSON $ aesonPrefix camelCase
instance FromForm PatchRequest

-- |Response entity for GetCollection
data ResGetCollection offset a = ResGetCollection
    { resSizeRemains :: Int
    , resNextOffset  :: Maybe offset
    , resCollection  :: a
    } deriving (Read, Show, Generic)

instance (NFData offset, NFData a) => NFData (ResGetCollection offset a)
instance (FromJSON offset, FromJSON a) => FromJSON (ResGetCollection offset a) where
    parseJSON = genericParseJSON $ aesonPrefix camelCase
instance (ToJSON offset, ToJSON a) => ToJSON (ResGetCollection offset a) where
    toJSON = genericToJSON $ aesonPrefix camelCase

{- |Smart constructor of `ResGetCollection`, specialized for `M.Map`.
    Perhaps DB should do this kind of things.
-}
makeResGetCollection :: Int -> Int -> M.Map k v -> ResGetCollection k (M.Map k v)
makeResGetCollection offset limit m =
    let dropped = M.drop offset m
        resSizeRemains = max 0 (M.size dropped - limit)
        (resCollection, rest) = M.splitAt limit dropped
        resNextOffset = fst <$> M.lookupMin rest
     in ResGetCollection {..}
