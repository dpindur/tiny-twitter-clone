{-# LANGUAGE FlexibleContexts, OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}

module HomeTimeline ( TimelineError (..)
                    , Tweet (..)
                    , getHomeTimeline
                    ) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Except
import Database.Redis
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 (pack, unpack, ByteString)
import GHC.Generics

data TimelineError = UsernameNotFound
                   | TokenInvalid
                   | DatabaseError
                   | PagingError
                   deriving (Show)

data Tweet = Tweet { userid   :: String
                   , username :: String
                   , time     :: Double
                   , body     :: String
                   } deriving (Show, Generic)
instance FromJSON Tweet
instance ToJSON Tweet

data Tweets = Tweets { tweets :: [Tweet]
                     , nextPage :: Maybe Integer
                     } deriving (Show, Generic)
instance FromJSON Tweets
instance ToJSON Tweets

pageSize :: Integer
pageSize = 25

getHomeTimeline :: String -> Integer -> IO (Either TimelineError Tweets)
getHomeTimeline token page = do
  conn <- connect defaultConnectInfo
  runRedis conn $ dbGetHomeTimeline token page

dbGetHomeTimeline :: (RedisCtx m (Either t), Control.Monad.IO.Class.MonadIO m) => String -> Integer -> m (Either TimelineError Tweets)
dbGetHomeTimeline token page =
  runExceptT $ do
    userid   <- ExceptT $ lookupToken token
    tweetids <- ExceptT $ getTweetIDs userid page
    tweets   <- mapM ExceptT $ fmap (lookupTweetID . unpack) tweetids
    case toInteger $ length tweets of
      x | x == 0        -> return $ Tweets [] Nothing
        | x < pageSize  -> return $ Tweets tweets Nothing
        | x >= pageSize -> return $ Tweets (init tweets) (Just (page + pageSize))
        | otherwise     -> throwE PagingError

getTweetIDs :: RedisCtx m (Either t) => String -> Integer -> m (Either TimelineError [ByteString])
getTweetIDs userid page = do
  let key = pack ("home_timeline:" ++ userid)
  tweets <- zrevrange key page (page + pageSize) -- Paging starts at zero so we return pageSize+1 to check for the next page
  return $ case tweets of
    Right ts -> Right ts
    Left _   -> Left DatabaseError

lookupTweetID :: RedisCtx m (Either t) => String -> m (Either TimelineError Tweet)
lookupTweetID id = do
  let key = pack ("tweets:" ++ id)
  tweet <- hmget key ["userid", "username", "time", "body"]
  let unpacked = (fmap . fmap . fmap) unpack tweet
  return $ case unpacked of
    Right [Just a, Just b, Just c, Just d] -> Right (Tweet a b (read c) d)
    Right _                                -> Left DatabaseError
    Left _                                 -> Left DatabaseError

lookupToken :: RedisCtx m (Either t) => String -> m (Either TimelineError String)
lookupToken token = do
  let key = pack ("token:" ++ token)
  id <- get (pack ("token:" ++ token))
  return $ case id of
    Right (Just id) -> Right (unpack id)
    Right (Nothing) -> Left TokenInvalid
    Left _          -> Left DatabaseError