{-# LANGUAGE FlexibleContexts, OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}

module User ( User (..)
            , UserError (..)
            , createUser
            , getUsers
            ) where

import Control.Arrow (left)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Except
import Database.Redis
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 (pack, unpack, ByteString)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics
import Crypto.BCrypt (hashPasswordUsingPolicy, fastBcryptHashingPolicy)
import Data.Maybe (fromMaybe)
import qualified Login as Login

data UserError = UsernameAlreadyExists
               | DatabaseError
               | PasswordHashError
               deriving (Show)

data User = User { userid :: String
                 , username :: String
                 } deriving (Show, Generic)
instance FromJSON User
instance ToJSON User

getTime :: IO Double
getTime = getPOSIXTime >>= return . realToFrac

getUsers :: IO (Either UserError [User])
getUsers = do
  conn <- connect defaultConnectInfo
  runRedis conn $ dbGetUsers

dbGetUsers :: (RedisCtx m (Either t), Control.Monad.IO.Class.MonadIO m) => m (Either UserError [User])
dbGetUsers =
  runExceptT $ do
    users <- ExceptT $ dbRetrieveUsers
    return $ fmap tupleToUser users
  where tupleToUser (x, y) = User y x

dbRetrieveUsers :: RedisCtx m (Either t) => m (Either UserError [(String, String)])
dbRetrieveUsers = do
  users <- hgetall "users"
  return $ case users of
    Right xs -> Right (fmap unpackTuple xs)
    Left _   -> Left DatabaseError
  where unpackTuple (x, y) = (unpack x, unpack y)

createUser :: String -> String -> IO (Either UserError Login.Token)
createUser username password = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    user <- dbCreateUser username password
    case user of
      Left err -> return $ Left err
      Right _  -> do
        token <- Login.dbCreateToken username password
        return $ left (\_ -> DatabaseError) $ token

dbCreateUser :: (RedisCtx m (Either t), Control.Monad.IO.Class.MonadIO m) => String -> String -> m (Either UserError Bool)
dbCreateUser username password =
  runExceptT $ do
    userexists  <- ExceptT $ lookupUserID username
    newUserID   <- ExceptT $ getNextUserID
    addNewUser  <- ExceptT $ dbInsertUser newUserID username password
    addUserList <- ExceptT $ dbInsertUserList newUserID username
    time        <- liftIO getTime
    -- Each user follows themselves
    addFollow   <- ExceptT $ addToFollowers newUserID newUserID time
    return True

lookupUserID :: RedisCtx m (Either t) => String -> m (Either UserError Bool)
lookupUserID username = do
  id <- hget "users" (pack username)
  return $ case id of
    Right (Just id) -> Left UsernameAlreadyExists
    Right (Nothing) -> Right True
    Left _          -> Left DatabaseError

getNextUserID :: RedisCtx m (Either t) => m (Either UserError String)
getNextUserID = do
  id <- incr "next_user_id"
  return $ case id of
    Right x -> Right (show x)
    Left _  -> Left DatabaseError

dbInsertUser :: (RedisCtx m (Either t), Control.Monad.IO.Class.MonadIO m) => String -> String -> String -> m (Either UserError Bool)
dbInsertUser userid username password = do
  let key = pack ("user:" ++ userid)
  passHash <- liftIO $ hashPasswordUsingPolicy fastBcryptHashingPolicy (pack password)
  case passHash of
    Nothing   -> return $ Left PasswordHashError
    Just pass -> do
      success <- hmset key [("username", pack username), ("password", pass)]
      return $ case success of
        Right _ -> Right True
        Left _  -> Left DatabaseError

dbInsertUserList :: RedisCtx m (Either t) => String -> String -> m (Either UserError Bool)
dbInsertUserList userid username = do
  success <- hset "users" (pack username) (pack userid)
  return $ case success of
    Right _ -> Right True
    Left _  -> Left DatabaseError

addToFollowers :: RedisCtx m (Either t) => String -> String -> Double -> m (Either UserError Bool)
addToFollowers idToFollow followerID time = do
  let key = pack ("followers:" ++ followerID)
  success <- zadd key [(time, pack idToFollow)]
  return $ case success of
    Right _ -> Right True
    Left _  -> Left DatabaseError