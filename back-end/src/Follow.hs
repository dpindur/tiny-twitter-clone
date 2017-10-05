{-# LANGUAGE FlexibleContexts, OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}

module Follow ( NewFollow (..)
              , FollowError (..)
              , createFollow
              , getFollowers
              , getFollowing
              ) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Except
import Database.Redis
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 (pack, unpack, ByteString)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics

import qualified User as User

data FollowError = TokenInvalid
                 | UsernameNotFound
                 | DatabaseError
                 deriving (Show)

data NewFollow = NewFollow { username :: String
                           } deriving (Show, Generic)
instance FromJSON NewFollow
instance ToJSON NewFollow

getTime :: IO Double
getTime = getPOSIXTime >>= return . realToFrac

createFollow :: String -> String -> IO (Either FollowError Bool)
createFollow username token = do
  conn <- connect defaultConnectInfo
  runRedis conn $ dbCreateFollow username token

dbCreateFollow :: (RedisCtx m (Either t), Control.Monad.IO.Class.MonadIO m) => String -> String -> m (Either FollowError Bool)
dbCreateFollow username token =
  runExceptT $ do
    followerID     <- ExceptT $ lookupToken token
    idToFollow     <- ExceptT $ lookupUserID username
    time           <- liftIO getTime
    addToFollowing <- ExceptT $ addToFollowing idToFollow followerID time
    addToFollowers <- ExceptT $ addToFollowers idToFollow followerID time
    return True

addToFollowing :: RedisCtx m (Either t) => String -> String -> Double -> m (Either FollowError Bool)
addToFollowing idToFollow followerID time = do
  let key = pack ("following:" ++ followerID)
  success <- zadd key [(time, pack idToFollow)]
  return $ case success of
    Right _ -> Right True
    Left _  -> Left DatabaseError

addToFollowers :: RedisCtx m (Either t) => String -> String -> Double -> m (Either FollowError Bool)
addToFollowers idToFollow followerID time = do
  let key = pack ("followers:" ++ idToFollow)
  success <- zadd key [(time, pack followerID)]
  return $ case success of
    Right _ -> Right True
    Left _  -> Left DatabaseError

lookupUserID :: RedisCtx m (Either t) => String -> m (Either FollowError String)
lookupUserID username = do
  id <- hget "users" (pack username)
  return $ case id of
    Right (Just id) -> Right (unpack id)
    Right (Nothing) -> Left UsernameNotFound
    Left _          -> Left DatabaseError

lookupToken :: RedisCtx m (Either t) => String -> m (Either FollowError String)
lookupToken token = do
  let key = pack ("token:" ++ token)
  id <- get (pack ("token:" ++ token))
  return $ case id of
    Right (Just id) -> Right (unpack id)
    Right (Nothing) -> Left TokenInvalid
    Left _          -> Left DatabaseError

getFollowers :: String -> IO (Either FollowError [User.User])
getFollowers username = do
  conn <- connect defaultConnectInfo
  runRedis conn $ dbGetFollowers username

dbGetFollowers :: (RedisCtx m (Either t), Control.Monad.IO.Class.MonadIO m) => String -> m (Either FollowError [User.User])
dbGetFollowers username =
  runExceptT $ do
    userid      <- ExceptT $ lookupUserID username
    followerIDs <- ExceptT $ dbGetFollowerIDs userid
    users       <- mapM ExceptT $ fmap (dbLookupUser . unpack) followerIDs
    return users

dbGetFollowerIDs :: RedisCtx m (Either t) => String -> m (Either FollowError [ByteString])
dbGetFollowerIDs userid = do
  let key = pack ("followers:" ++ userid)
  users <- zrevrange key 0 (-1)
  return $ case users of
    Right us -> Right us
    Left _   -> Left DatabaseError

dbLookupUser :: RedisCtx m (Either t) => String -> m (Either FollowError User.User)
dbLookupUser userid = do
  let key = pack ("user:" ++ userid)
  user <- hmget key ["username"]
  let unpacked = (fmap . fmap . fmap) unpack user
  return $ case unpacked of
    Right [Just name] -> Right (User.User userid name)
    Right _           -> Left DatabaseError
    Left _            -> Left DatabaseError

getFollowing :: String -> IO (Either FollowError [User.User])
getFollowing username = do
  conn <- connect defaultConnectInfo
  runRedis conn $ dbGetFollowing username

dbGetFollowing :: (RedisCtx m (Either t), Control.Monad.IO.Class.MonadIO m) => String -> m (Either FollowError [User.User])
dbGetFollowing username =
  runExceptT $ do
    userid       <- ExceptT $ lookupUserID username
    followingIDs <- ExceptT $ dbGetFollowingIDs userid
    users        <- mapM ExceptT $ fmap (dbLookupUser . unpack) followingIDs
    return users

dbGetFollowingIDs :: RedisCtx m (Either t) => String -> m (Either FollowError [ByteString])
dbGetFollowingIDs userid = do
  let key = pack ("following:" ++ userid)
  users <- zrevrange key 0 (-1)
  return $ case users of
    Right us -> Right us
    Left _   -> Left DatabaseError

{-
-- a unfollows b
-- Need to remove tweets from users timeline
unfollow :: String -> String -> IO ()
unfollow a b = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    let akey = BS.pack $ "following:" ++ a
    let bkey = BS.pack $ "followers:" ++ b
    let apack = BS.pack a
    let bpack = BS.pack b
    aunfollow <- zrem akey [bpack]
    bunfollow <- zrem bkey [apack]
    liftIO $ print bunfollow
-}