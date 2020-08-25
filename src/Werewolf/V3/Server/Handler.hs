{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Werewolf.V3.Server.Handler where

import           Control.Monad
import           Control.Monad.Error.Class           (MonadError, throwError)
import           Control.Monad.Random.Class          (MonadRandom (..))
import           Data.Foldable
import qualified Data.Map                            as M
import           Data.Maybe                          (catMaybes, fromMaybe)
import qualified Data.Set                            as S
import qualified Data.Text                           as T
import qualified Data.Text.Lazy                      as LT
import qualified Data.Text.Lazy.Encoding             as LT
import           Servant.API
import           Servant.Server
import           Werewolf.Utils                      (groupedShuffle)
import           Werewolf.V3.Deck                    (Deck (..))
import qualified Werewolf.V3.DeckDao.Class           as Dao
import           Werewolf.V3.History
import qualified Werewolf.V3.HistoryDao.Class        as Dao
import           Werewolf.V3.Seiheki
import           Werewolf.V3.SeihekiComment
import qualified Werewolf.V3.SeihekiCommentDao.Class as Dao
import qualified Werewolf.V3.SeihekiDao.Class        as Dao
import           Werewolf.V3.Server.API


accessControlAllowOrigin :: String
accessControlAllowOrigin = "*"  -- "rinse.github.io"

handler :: (MonadError ServerError m, MonadRandom m
        , Dao.MonadSeihekiDao m
        , Dao.MonadSeihekiCommentDao m
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

postSeihekis :: (Monad m, Dao.MonadSeihekiDao m)
             => Seiheki -> m (Headers '[Header "Access-Control-Allow-Origin" String, Header "Location" String] (Res201 SeihekiId))
postSeihekis seiheki = do
    seihekiId <- Dao.postSeiheki seiheki
    return . addHeader accessControlAllowOrigin $ addHeader ("/v3/seihekis" /~ show seihekiId) (res201 seihekiId)

getSeihekis :: (MonadError ServerError m, Dao.MonadSeihekiDaoReadOnly m)
            => Maybe T.Text -> Maybe Int -> Maybe Int -> m (Headers '[AccessControlAllowOriginHeader] (ResGetCollection SeihekiId SeihekiMap))
getSeihekis author offset limit = do
    for_ limit $ validateLimitation 100
    seihekiMap <- Dao.getSeihekis $ \s -> maybe True (== seihekiAuthor s) author
    let offset' = fromMaybe defaultOffset offset
        limit' = fromMaybe defaultLimit limit
    return . addHeader accessControlAllowOrigin $ makeResGetCollection offset' limit' seihekiMap

getSeiheki :: Dao.MonadSeihekiDaoReadOnly m => SeihekiId -> m Seiheki
getSeiheki = Dao.lookupSeiheki

postSeihekiComments :: (Monad m, Dao.MonadSeihekiDao m, Dao.MonadSeihekiCommentDao m)
                    => SeihekiId -> SeihekiComment
                    -> m (Headers '[Header "Location" String] (Res201 SeihekiCommentId))
postSeihekiComments seihekiId seihekiComment = do
    _ <- Dao.lookupSeiheki seihekiId
    seihekiCommentId <- Dao.postSeihekiComment seihekiComment
    _ <- flip Dao.patchSeiheki seihekiId $ \s@Seiheki {..} ->
        s {seihekiCommentIds = seihekiCommentId:seihekiCommentIds}
    return $ addHeader ("/v3/seihekis" /~ show seihekiId /~ "comments" /~ show seihekiCommentId) (res201 seihekiCommentId)

getSeihekiComments :: (MonadError ServerError m, Dao.MonadSeihekiDaoReadOnly m, Dao.MonadSeihekiCommentDaoReadOnly m)
                   => SeihekiId -> Maybe Int -> Maybe Int -> m (ResGetCollection SeihekiCommentId SeihekiCommentMap)
getSeihekiComments seihekiId offset limit = do
    Seiheki {seihekiCommentIds = commentIds} <- Dao.lookupSeiheki seihekiId -- may throw 404
    for_ limit $ validateLimitation 100
    commentMap <- Dao.getSeihekiComments
    let commentMap' = M.restrictKeys commentMap (S.fromList commentIds)
        offset' = fromMaybe defaultOffset offset
        limit' = fromMaybe defaultLimit limit
    return $ makeResGetCollection offset' limit' commentMap'

getSeihekiComment :: Dao.MonadSeihekiCommentDaoReadOnly m => SeihekiId -> SeihekiCommentId -> m SeihekiComment
getSeihekiComment _ = Dao.lookupSeihekiComment

optionsSeihekiUpvotes :: (Monad m, Dao.MonadSeihekiDaoReadOnly m)
                      => SeihekiId -> m (OptionsHeaders NoContent)
optionsSeihekiUpvotes seihekiId = do
    _ <- Dao.lookupSeiheki seihekiId
    return
         . addHeader accessControlAllowOrigin
         . addHeader "PATCH, OPTIONS"
         . addHeader "*"
         . addHeader 600
         $ addHeader "Origin" NoContent

patchSeihekiUpvotes :: (Monad m, MonadError ServerError m, Dao.MonadSeihekiDao m)
                   => SeihekiId -> PatchRequest -> m (Headers '[AccessControlAllowOriginHeader] NoContent)
patchSeihekiUpvotes seihekiId PatchRequest {..} = do
    when (patchOp /= "increment") $
        throwError err400 {errBody = LT.encodeUtf8 . LT.fromStrict $ patchOp <> " is not allowed as op."}
    Dao.patchSeiheki incrementUpvotes seihekiId
    return $ addHeader accessControlAllowOrigin NoContent
    where
    incrementUpvotes s@Seiheki {seihekiUpvotes=upvotes} = s {seihekiUpvotes = upvotes + 1}

postCards :: (MonadRandom m, Dao.MonadSeihekiDaoReadOnly m, Dao.MonadDeckDao m, Dao.MonadHistoryDaoReadOnly m)
          => m (Headers '[AccessControlAllowOriginHeader] NoContent)
postCards = do
    history <- unHistory <$> Dao.getHistory
    seihekiMap <- flip M.withoutKeys (S.fromList history) <$> Dao.getSeihekis (const True)
    seihekiMap' <- groupedShuffle (seihekiAuthor . snd) (M.assocs seihekiMap)
    Dao.putDeck . Deck $ fst <$> seihekiMap'
    return $ addHeader accessControlAllowOrigin NoContent

getCards :: (MonadError ServerError m, Dao.MonadSeihekiDaoReadOnly m, Dao.MonadDeckDaoReadOnly m)
         => Maybe Int -> Maybe Int
         -> m (Headers '[AccessControlAllowOriginHeader] (ResGetCollection SeihekiId [(SeihekiId, Seiheki)]))
getCards offset limit = do
    for_ limit $ validateLimitation 100
    seihekiIds <- unDeck <$> Dao.getDeck
    seihekiMap <- catMaybes <$> withSeihekiBody seihekiIds
    let offset' = fromMaybe defaultOffset offset
        limit' = fromMaybe defaultLimit limit
    return . addHeader accessControlAllowOrigin $ makeResGetCollection' offset' limit' seihekiMap

optionsCard :: (Monad m, Dao.MonadSeihekiDaoReadOnly m) => SeihekiId -> m (OptionsHeaders NoContent)
optionsCard  seihekiId = do
    _ <- Dao.lookupSeiheki seihekiId
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

getHistories :: (MonadError ServerError m, Dao.MonadSeihekiDaoReadOnly m, Dao.MonadHistoryDaoReadOnly m)
             => Maybe Int -> Maybe Int
             -> m (Headers '[AccessControlAllowOriginHeader] (ResGetCollection SeihekiId [(SeihekiId, Seiheki)]))
getHistories offset limit = do
    for_ limit $ validateLimitation 100
    seihekiIds <- unHistory <$> Dao.getHistory
    seihekiMap <- catMaybes <$> withSeihekiBody seihekiIds
    let offset' = fromMaybe defaultOffset offset
        limit' = fromMaybe defaultLimit limit
    return . addHeader accessControlAllowOrigin $ makeResGetCollection' offset' limit' seihekiMap

withSeihekiBody :: (Monad m, Dao.MonadSeihekiDaoReadOnly m) => [SeihekiId] -> m [Maybe (SeihekiId, Seiheki)]
withSeihekiBody seihekiIds = do
    allSeihekiMap <- Dao.getSeihekis (const True)
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
