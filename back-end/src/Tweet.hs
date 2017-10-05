{-# LANGUAGE FlexibleContexts, OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}

module Tweet ( NewTweet (..)
             , TweetError (..)
             , createTweet
             ) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Except
import Database.Redis
import Data.Aeson (FromJSON, ToJSON)
import Data.Double.Conversion.ByteString
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.ByteString.Char8 (pack, unpack, ByteString)
import GHC.Generics
import HomeTimeline (Tweet(..))

data TweetError = TokenInvalid
                | UserIDNotFound
                | DatabaseError
                | TweetTooLong
                deriving (Show)

data NewTweet = NewTweet { body :: String
                         } deriving (Show, Generic)
instance FromJSON NewTweet
instance ToJSON NewTweet

getTime :: IO Double
getTime = getPOSIXTime >>= return . realToFrac

createTweet :: String -> String -> IO (Either TweetError Tweet)
createTweet tweet token = do
  conn <- connect defaultConnectInfo
  runRedis conn $ dbCreateTweet tweet token

dbCreateTweet :: (RedisCtx m (Either t), Control.Monad.IO.Class.MonadIO m) => String -> String -> m (Either TweetError Tweet)
dbCreateTweet body token =
  runExceptT $ do
    if length body > 150 then
      throwE TweetTooLong
    else do
      userid       <- ExceptT $ lookupToken token
      username     <- ExceptT $ lookupUsername userid
      time         <- liftIO getTime
      tweetid      <- ExceptT $ getTweetID
      followers    <- ExceptT $ getFollowers userid
      -- create tweet
      added        <- ExceptT $ dbAddTweet tweetid userid username body time
      -- add to user timeline
      userTimeline <- ExceptT $ dbAddToUserTimeline tweetid time userid
      -- add to home timelines of followers
      homeTimeline <- mapM ExceptT $ fmap (dbAddtoHomeTimeline tweetid time) followers
      return added

dbAddTweet :: RedisCtx m (Either t) => String -> String -> String -> String -> Double-> m (Either TweetError Tweet)
dbAddTweet tweetid userid username body time = do
  let key = pack ("tweets:" ++ tweetid)
  success <- hmset key [("username", pack username)
                       ,("userid",   pack userid)
                       ,("body",     pack body)
                       ,("time",     toShortest time)]
  return $ case success of
    Right _ -> Right $ Tweet userid username time body
    Left _  -> Left DatabaseError

dbAddToUserTimeline :: RedisCtx m (Either t) => String -> Double -> String -> m (Either TweetError Bool)
dbAddToUserTimeline tweetid time userid = do
  let key = pack ("user_timeline:" ++ userid)
  success <- zadd key [(time, pack tweetid)]
  return $ case success of
    Right _ -> Right True
    Left _  -> Left DatabaseError

dbAddtoHomeTimeline :: RedisCtx m (Either t) => String -> Double -> String -> m (Either TweetError Bool)
dbAddtoHomeTimeline tweetid time userid = do
  let key = pack ("home_timeline:" ++ userid)
  success <- zadd key [(time, pack tweetid)]
  return $ case success of
    Right _ -> Right True
    Left _  -> Left DatabaseError

getFollowers :: RedisCtx m (Either t) => String -> m (Either TweetError [String])
getFollowers userid = do
  let key = pack ("followers:" ++ userid)
  followers <- zrange key 0 (-1)
  return $ case followers of
    Right x -> Right (fmap unpack x)
    Left _  -> Left DatabaseError

getTweetID :: RedisCtx m (Either t) => m (Either TweetError String)
getTweetID = do
  id <- incr "next_tweet_id"
  return $ case id of
    Right x -> Right (show x)
    Left _  -> Left DatabaseError

lookupToken :: RedisCtx m (Either t) => String -> m (Either TweetError String)
lookupToken token = do
  let key = pack ("token:" ++ token)
  id <- get (pack ("token:" ++ token))
  return $ case id of
    Right (Just id) -> Right (unpack id)
    Right (Nothing) -> Left TokenInvalid
    Left _          -> Left DatabaseError

lookupUsername :: RedisCtx m (Either t) => String -> m (Either TweetError String)
lookupUsername userid = do
  let key = pack ("user:" ++ userid)
  username <- hmget key ["username"]
  return $ case username of
    Right [Just name] -> Right (unpack name)
    Right _           -> Left UserIDNotFound
    Left _            -> Left DatabaseError