{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Werewolf.V3.Server.Handler where

import           Control.Monad
import           Control.Monad.Error.Class    (MonadError, throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Random.Class   (MonadRandom (..))
import qualified Data.Acid                    as A
import qualified Data.Acid.Advanced           as A
import           Data.Foldable
import qualified Data.Map                     as M
import           Data.Maybe                   (catMaybes, fromMaybe)
import qualified Data.Set                     as S
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as LT
import qualified Data.Text.Lazy.Encoding      as LT
import           Servant.API
import           Servant.Server
import           Werewolf.Utils               (groupedShuffle)
import qualified Werewolf.V3.DB               as DB
import qualified Werewolf.V3.DB.Class         as DB
import           Werewolf.V3.Deck             (Deck (..))
import           Werewolf.V3.History
import           Werewolf.V3.Seiheki
import           Werewolf.V3.SeihekiComment
import           Werewolf.V3.Server.API


accessControlAllowOrigin :: String
accessControlAllowOrigin = "*"  -- "rinse.github.io"

handler :: (MonadIO m, MonadError ServerError m, MonadRandom m
        , DB.MonadDB m
        ) => ServerT API m
handler = postSeihekis :<|> getSeihekis
    :<|> getSeiheki
    :<|> optionsSeihekiUpvotes :<|> patchSeihekiUpvotes
    :<|> postSeihekiComments :<|> getSeihekiComments
    :<|> getSeihekiComment
    :<|> optionsSeihekiCommentUpvotes :<|> patchSeihekiCommentUpvotes
    :<|> postCards :<|> getCards
    :<|> optionsCard :<|> deleteCard
    :<|> getHistories

postSeihekis :: (MonadIO m, DB.MonadDB m)
             => Seiheki -> m (Headers '[Header "Access-Control-Allow-Origin" String, Header "Location" String] (Res201 SeihekiId))
postSeihekis seiheki = do
    db <- DB.getAcidState
    seihekiId <- A.update' db $ DB.PostSeiheki seiheki
    liftIO $ A.createCheckpoint db
    return . addHeader accessControlAllowOrigin $ addHeader ("/v3/seihekis" /~ show seihekiId) (res201 seihekiId)

getSeihekis :: (MonadIO m, MonadError ServerError m, DB.MonadDB m)
            => Maybe T.Text -> Maybe Int -> Maybe Int -> [String]
            -> m (Headers '[AccessControlAllowOriginHeader] (ResGetCollection SeihekiId [(SeihekiId, Seiheki)]))
getSeihekis author offset limit q = do
    for_ limit $ validateLimitation 100
    db <- DB.getAcidState
    seihekiMap <- A.query' db DB.GetSeihekis
    let seihekiMap' = M.filter (\s -> maybe True (== seihekiAuthor s) author) seihekiMap
    seihekiMap'' <- if "exclude-history" `elem` q
        then do
            history <- unHistory <$> A.query' db DB.GetHistory
            return $ M.withoutKeys seihekiMap' (S.fromList history)
        else return seihekiMap'
    let offset' = fromMaybe defaultOffset offset
        limit' = fromMaybe defaultLimit limit
    return . addHeader accessControlAllowOrigin . makeResGetCollection' offset' limit' $ M.toAscList seihekiMap''

getSeiheki :: (MonadIO m, MonadError ServerError m, DB.MonadDB m) => SeihekiId -> m (Headers '[AccessControlAllowOriginHeader] Seiheki)
getSeiheki seihekiId = do
    db <- DB.getAcidState
    seiheki <- A.query' db $ DB.LookupSeiheki seihekiId
    seiheki' <- maybe (throwError err404) return seiheki
    return $ addHeader accessControlAllowOrigin seiheki'

optionsSeihekiUpvotes :: (MonadIO m, MonadError ServerError m, DB.MonadDB m)
                      => SeihekiId -> m (OptionsHeaders NoContent)
optionsSeihekiUpvotes seihekiId = do
    db <- DB.getAcidState
    seiheki <- A.query' db $ DB.LookupSeiheki seihekiId
    case seiheki of
        Nothing -> throwError err404
        Just _ -> return ()
    return
         . addHeader accessControlAllowOrigin
         . addHeader "PATCH, OPTIONS"
         . addHeader "*"
         . addHeader 600
         $ addHeader "Origin" NoContent

patchSeihekiUpvotes :: (MonadIO m, MonadError ServerError m, DB.MonadDB m)
                   => SeihekiId -> PatchRequest -> m (Headers '[AccessControlAllowOriginHeader] NoContent)
patchSeihekiUpvotes seihekiId PatchRequest {..} = do
    when (patchOp /= "increment") $
        throwError err400 {errBody = LT.encodeUtf8 . LT.fromStrict $ patchOp <> " is not allowed as op."}
    db <- DB.getAcidState
    seiheki <- A.query' db $ DB.LookupSeiheki seihekiId
    seiheki'@Seiheki{..} <- maybe (throwError err404) return seiheki
    A.update' db $ DB.InsertSeiheki seihekiId seiheki' {seihekiUpvotes = seihekiUpvotes + 1}
    return $ addHeader accessControlAllowOrigin NoContent

postSeihekiComments :: (MonadIO m, MonadError ServerError m, DB.MonadDB m)
                    => SeihekiId -> SeihekiComment
                    -> m (Headers '[AccessControlAllowOriginHeader, LocationHeader] (Res201 SeihekiCommentId))
postSeihekiComments seihekiId seihekiComment = do
    db <- DB.getAcidState
    seiheki <- A.query' db $ DB.LookupSeiheki seihekiId
    seiheki'@Seiheki {..} <- maybe (throwError err404) return seiheki
    seihekiCommentId <- A.update' db (DB.PostSeihekiComment seihekiComment)
    A.update' db $ DB.InsertSeiheki seihekiId (seiheki'{seihekiCommentIds = seihekiCommentId:seihekiCommentIds})
    liftIO $ A.createCheckpoint db
    return
        . addHeader accessControlAllowOrigin
        $ addHeader ("/v3/seihekis" /~ show seihekiId /~ "comments" /~ show seihekiCommentId) (res201 seihekiCommentId)

getSeihekiComments :: (MonadIO m, MonadError ServerError m, DB.MonadDB m)
                   => SeihekiId -> Maybe Int -> Maybe Int -> m (ResGetCollection SeihekiCommentId [(SeihekiCommentId, SeihekiComment)])
getSeihekiComments seihekiId offset limit = do
    db <- DB.getAcidState
    seiheki <- A.query' db $ DB.LookupSeiheki seihekiId
    Seiheki {seihekiCommentIds = commentIds} <- maybe (throwError err404) return seiheki
    for_ limit $ validateLimitation 100
    commentMap <- A.query' db DB.GetSeihekiComments
    let commentMap' = M.toAscList $ M.restrictKeys commentMap (S.fromList commentIds)
        offset' = fromMaybe defaultOffset offset
        limit' = fromMaybe defaultLimit limit
    return $ makeResGetCollection' offset' limit' commentMap'

getSeihekiComment :: (MonadIO m, MonadError ServerError m, DB.MonadDB m)
                  => SeihekiId -> SeihekiCommentId -> m (Headers '[AccessControlAllowOriginHeader] SeihekiComment)
getSeihekiComment _ seihekiCommentId = do
    db <- DB.getAcidState
    seihekiComment <- A.query' db $ DB.LookupSeihekiComment seihekiCommentId
    seihekiComment' <- maybe (throwError err404) return seihekiComment
    return $ addHeader accessControlAllowOrigin seihekiComment'

optionsSeihekiCommentUpvotes :: (MonadIO m, MonadError ServerError m, DB.MonadDB m)
                             => SeihekiId -> SeihekiCommentId -> m (OptionsHeaders NoContent)
optionsSeihekiCommentUpvotes _ seihekiCommentId = do
    db <- DB.getAcidState
    seihekiComment <- A.query' db $ DB.LookupSeihekiComment seihekiCommentId
    case seihekiComment of
        Nothing -> throwError err404
        Just _ -> return ()
    return
         . addHeader accessControlAllowOrigin
         . addHeader "PATCH, OPTIONS"
         . addHeader "*"
         . addHeader 600
         $ addHeader "Origin" NoContent

patchSeihekiCommentUpvotes :: (MonadIO m, MonadError ServerError m, DB.MonadDB m)
                           => SeihekiId -> SeihekiCommentId -> PatchRequest
                           -> m (Headers '[AccessControlAllowOriginHeader] NoContent)
patchSeihekiCommentUpvotes _ seihekiCommentId PatchRequest {..} = do
    when (patchOp /= "increment") $
        throwError err400 {errBody = LT.encodeUtf8 . LT.fromStrict $ patchOp <> " is not allowed as op."}
    db <- DB.getAcidState
    seihekiComment <- A.query' db $ DB.LookupSeihekiComment seihekiCommentId
    seihekiComment'@SeihekiComment{..} <- maybe (throwError err404) return seihekiComment
    A.update' db $ DB.InsertSeihekiComment seihekiCommentId seihekiComment' {commentUpvotes = commentUpvotes + 1}
    return $ addHeader accessControlAllowOrigin NoContent

postCards :: (MonadIO m, MonadRandom m, DB.MonadDB m)
          => m (Headers '[AccessControlAllowOriginHeader] NoContent)
postCards = do
    db <- DB.getAcidState
    history <- unHistory <$> A.query' db DB.GetHistory
    seihekiMap <- flip M.withoutKeys (S.fromList history) <$> A.query' db DB.GetSeihekis
    seihekiMap' <- groupedShuffle (seihekiAuthor . snd) (M.assocs seihekiMap)
    A.update' db . DB.PutDeck . Deck $ fst <$> seihekiMap'
    return $ addHeader accessControlAllowOrigin NoContent

getCards :: (MonadIO m, MonadError ServerError m, DB.MonadDB m)
         => Maybe Int -> Maybe Int
         -> m (Headers '[AccessControlAllowOriginHeader] (ResGetCollection SeihekiId [(SeihekiId, Seiheki)]))
getCards offset limit = do
    for_ limit $ validateLimitation 100
    db <- DB.getAcidState
    seihekiIds <- unDeck <$> A.query' db DB.GetDeck
    seihekiMap <- catMaybes <$> withSeihekiBody seihekiIds
    let offset' = fromMaybe defaultOffset offset
        limit' = fromMaybe defaultLimit limit
    return . addHeader accessControlAllowOrigin $ makeResGetCollection' offset' limit' seihekiMap

optionsCard :: (MonadIO m, MonadError ServerError m, DB.MonadDB m) => SeihekiId -> m (OptionsHeaders NoContent)
optionsCard  seihekiId = do
    db <- DB.getAcidState
    seiheki <- A.query' db $ DB.LookupSeiheki seihekiId
    case seiheki of
        Nothing -> throwError err404
        Just _ -> return ()
    return
         . addHeader accessControlAllowOrigin
         . addHeader "DELETE, OPTIONS"
         . addHeader "*"
         . addHeader 600
         $ addHeader "Origin" NoContent

deleteCard :: (MonadIO m, MonadError ServerError m, DB.MonadDB m) => SeihekiId -> m NoContent
deleteCard seihekiId = do
    db <- DB.getAcidState
    deck <- unDeck <$> A.query' db DB.GetDeck
    let (h, t) = break (== seihekiId) deck
    newDeck <- case t of
        [] -> throwError err404
        (x:xs) -> do
            A.update' db $ DB.AddHistory x
            return $ h <> xs
    A.update' db . DB.PutDeck $ Deck newDeck
    return NoContent

getHistories :: (MonadIO m, MonadError ServerError m, DB.MonadDB m)
             => Maybe Int -> Maybe Int
             -> m (Headers '[AccessControlAllowOriginHeader] (ResGetCollection SeihekiId [(SeihekiId, Seiheki)]))
getHistories offset limit = do
    for_ limit $ validateLimitation 100
    db <- DB.getAcidState
    seihekiIds <- unHistory <$> A.query' db DB.GetHistory
    seihekiMap <- catMaybes <$> withSeihekiBody seihekiIds
    let offset' = fromMaybe defaultOffset offset
        limit' = fromMaybe defaultLimit limit
    return . addHeader accessControlAllowOrigin $ makeResGetCollection' offset' limit' seihekiMap

withSeihekiBody :: (MonadIO m, DB.MonadDB m) => [SeihekiId] -> m [Maybe (SeihekiId, Seiheki)]
withSeihekiBody seihekiIds = do
    db <- DB.getAcidState
    allSeihekiMap <- A.query' db DB.GetSeihekis
    return $ do
        seihekiId <- seihekiIds
        return $ do
            seiheki <- M.lookup seihekiId allSeihekiMap
            return (seihekiId, seiheki)

defaultLimit :: Int
defaultLimit = 100

defaultOffset :: Int
defaultOffset = 0

validateLimitation :: MonadError ServerError m => Int -> Int -> m ()
validateLimitation limitation requested =
    when (limitation < requested) $ do
        let msg = "Requested number of resources exceeds the limitation."
        throwError err400 {errBody = msg}

(/~) :: String -> String -> String
s /~ t = s <> ('/': t)
