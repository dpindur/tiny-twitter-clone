{-# LANGUAGE FlexibleContexts, OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}

module UserTimeline ( TimelineError (..)
                    , Tweet (..)
                    , getUserTimeline
                    ) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Except
import Database.Redis
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 (pack, unpack, ByteString)
import GHC.Generics

data TimelineError = UsernameNotFound
                   | DatabaseError
                   deriving (Show)

data Tweet = Tweet { userid   :: String
                   , username :: String
                   , time     :: Double
                   , body     :: String
                   } deriving (Show, Generic)
instance FromJSON Tweet
instance ToJSON Tweet

getUserTimeline :: String -> IO (Either TimelineError [Tweet])
getUserTimeline username = do
  conn <- connect defaultConnectInfo
  runRedis conn $ dbGetUserTimeline username

dbGetUserTimeline :: (RedisCtx m (Either t), Control.Monad.IO.Class.MonadIO m) => String -> m (Either TimelineError [Tweet])
dbGetUserTimeline username =
  runExceptT $ do
    userid   <- ExceptT $ lookupUserID username
    tweetids <- ExceptT $ getTweetIDs userid
    tweets   <- mapM ExceptT $ fmap (lookupTweetID . unpack) tweetids
    return tweets

lookupUserID :: RedisCtx m (Either t) => String -> m (Either TimelineError String)
lookupUserID username = do
  id <- hget "users" (pack username)
  return $ case id of
    Right (Just id) -> Right (unpack id)
    Right (Nothing) -> Left UsernameNotFound
    Left _          -> Left DatabaseError

getTweetIDs :: RedisCtx m (Either t) => String -> m (Either TimelineError [ByteString])
getTweetIDs userid = do
  let key = pack ("user_timeline:" ++ userid)
  tweets <- zrevrange key 0 (-1)
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