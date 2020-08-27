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
import qualified Werewolf.V3.DeckDao.Class    as Dao
import           Werewolf.V3.History
import qualified Werewolf.V3.HistoryDao.Class as Dao
import           Werewolf.V3.Seiheki
import           Werewolf.V3.SeihekiComment
import           Werewolf.V3.Server.API


accessControlAllowOrigin :: String
accessControlAllowOrigin = "*"  -- "rinse.github.io"

handler :: (MonadIO m, MonadError ServerError m, MonadRandom m
        , DB.MonadDB m
        , Dao.MonadDeckDao m
        , Dao.MonadHistoryDao m
        ) => ServerT API m
handler = postSeihekis :<|> getSeihekis
    :<|> getSeiheki
    :<|> postSeihekiComments :<|> getSeihekiComments
    :<|> getSeihekiComment
    :<|> optionsSeihekiUpvotes :<|> patchSeihekiUpvotes
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
            => Maybe T.Text -> Maybe Int -> Maybe Int -> m (Headers '[AccessControlAllowOriginHeader] (ResGetCollection SeihekiId SeihekiMap))
getSeihekis author offset limit = do
    for_ limit $ validateLimitation 100
    db <- DB.getAcidState
    seihekiMap <- A.query' db DB.GetSeihekis
    let seihekiMap' = M.filter (\s -> maybe True (== seihekiAuthor s) author) seihekiMap
        offset' = fromMaybe defaultOffset offset
        limit' = fromMaybe defaultLimit limit
    return . addHeader accessControlAllowOrigin $ makeResGetCollection offset' limit' seihekiMap'

getSeiheki :: (MonadIO m, MonadError ServerError m, DB.MonadDB m) => SeihekiId -> m Seiheki
getSeiheki seihekiId = do
    db <- DB.getAcidState
    seiheki <- A.query' db $ DB.LookupSeiheki seihekiId
    maybe (throwError err404) return seiheki

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
                   => SeihekiId -> Maybe Int -> Maybe Int -> m (ResGetCollection SeihekiCommentId SeihekiCommentMap)
getSeihekiComments seihekiId offset limit = do
    db <- DB.getAcidState
    seiheki <- A.query' db $ DB.LookupSeiheki seihekiId
    Seiheki {seihekiCommentIds = commentIds} <- maybe (throwError err404) return seiheki
    for_ limit $ validateLimitation 100
    commentMap <- A.query' db DB.GetSeihekiComments
    let commentMap' = M.restrictKeys commentMap (S.fromList commentIds)
        offset' = fromMaybe defaultOffset offset
        limit' = fromMaybe defaultLimit limit
    return $ makeResGetCollection offset' limit' commentMap'

getSeihekiComment :: (MonadIO m, MonadError ServerError m, DB.MonadDB m)
                  => SeihekiId -> SeihekiCommentId -> m (Headers '[AccessControlAllowOriginHeader] SeihekiComment)
getSeihekiComment _ seihekiCommentId = do
    db <- DB.getAcidState
    seihekiComment <- A.query' db $ DB.LookupSeihekiComment seihekiCommentId
    seihekiComment' <- maybe (throwError err404) return seihekiComment
    return $ addHeader accessControlAllowOrigin seihekiComment'

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

postCards :: (MonadIO m, MonadRandom m, DB.MonadDB m, Dao.MonadDeckDao m, Dao.MonadHistoryDaoReadOnly m)
          => m (Headers '[AccessControlAllowOriginHeader] NoContent)
postCards = do
    db <- DB.getAcidState
    history <- unHistory <$> Dao.getHistory
    seihekiMap <- flip M.withoutKeys (S.fromList history) <$> A.query' db DB.GetSeihekis
    seihekiMap' <- groupedShuffle (seihekiAuthor . snd) (M.assocs seihekiMap)
    Dao.putDeck . Deck $ fst <$> seihekiMap'
    return $ addHeader accessControlAllowOrigin NoContent

getCards :: (MonadIO m, MonadError ServerError m, DB.MonadDB m, Dao.MonadDeckDaoReadOnly m)
         => Maybe Int -> Maybe Int
         -> m (Headers '[AccessControlAllowOriginHeader] (ResGetCollection SeihekiId [(SeihekiId, Seiheki)]))
getCards offset limit = do
    for_ limit $ validateLimitation 100
    seihekiIds <- unDeck <$> Dao.getDeck
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

deleteCard :: (MonadError ServerError m, Dao.MonadDeckDao m, Dao.MonadHistoryDao m) => SeihekiId -> m NoContent
deleteCard seihekiId = do
    deck <- unDeck <$> Dao.getDeck
    let (h, t) = break (== seihekiId) deck
    newDeck <- case t of
        [] -> throwError err404
        (x:xs) -> do
            Dao.addHistory x
            return $ h <> xs
    Dao.putDeck $ Deck newDeck
    return NoContent

getHistories :: (MonadIO m, MonadError ServerError m, DB.MonadDB m, Dao.MonadHistoryDaoReadOnly m)
             => Maybe Int -> Maybe Int
             -> m (Headers '[AccessControlAllowOriginHeader] (ResGetCollection SeihekiId [(SeihekiId, Seiheki)]))
getHistories offset limit = do
    for_ limit $ validateLimitation 100
    seihekiIds <- unHistory <$> Dao.getHistory
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
