{-# LANGUAGE FlexibleContexts, OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}

module Login ( LoginError (..)
             , Token (..)
             , LoginRequest (..)
             , createLoginToken
             , dbCreateToken) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Except
import Database.Redis
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Base64 (encode)
import Data.ByteString.Char8 (pack, unpack)
import GHC.Generics
import OpenSSL.Random (randBytes)
import Crypto.BCrypt (validatePassword)

data LoginError = UsernameNotFound
                | PasswordNotFound
                | IncorrectPassword
                | DatabaseError
                deriving (Show)

data Token = Token { token :: String
                   } deriving (Show, Generic)
instance FromJSON Token
instance ToJSON Token

data LoginRequest = LoginRequest { username :: String
                                 , password :: String
                                 } deriving (Show, Generic)
instance FromJSON LoginRequest
instance ToJSON LoginRequest

createLoginToken :: String -> String -> IO (Either LoginError Token)
createLoginToken username password = do
  conn <- connect defaultConnectInfo
  runRedis conn $ dbCreateToken username password

dbCreateToken :: (RedisCtx m (Either t), Control.Monad.IO.Class.MonadIO m) => String -> String -> m (Either LoginError Token)
dbCreateToken username password =
  runExceptT $ do
    userid <- ExceptT $ lookupUserID username
    dbPass   <- ExceptT $ lookupPassword userid
    if validatePassword (pack dbPass) (pack password) then do
      tok <- liftIO $ randBytes 32
      let token = unpack (encode tok)
      saveToken <- ExceptT $ saveToken userid token
      return $ Token token
    else
      throwE IncorrectPassword

saveToken :: RedisCtx m (Either t) => String -> String -> m (Either LoginError Bool)
saveToken userid token = do
  let key = pack $ "token:" ++ token
  set key (pack userid)
  success <- expire key 1800
  return $ case success of
    Right True  -> Right True
    Right False -> Left DatabaseError
    Left _      -> Left DatabaseError

lookupUserID :: RedisCtx m (Either t) => String -> m (Either LoginError String)
lookupUserID username = do
  id <- hget "users" (pack username)
  return $ case id of
    Right (Just id) -> Right (unpack id)
    Right (Nothing) -> Left UsernameNotFound
    Left _          -> Left DatabaseError

lookupPassword :: RedisCtx m (Either t) => String -> m (Either LoginError String)
lookupPassword userid = do
  let key = pack $ "user:" ++ userid
  pass <- hmget key ["password"]
  return $ case pass of
    Right [Just p]  -> Right (unpack p)
    Right [Nothing] -> Left PasswordNotFound
    _               -> Left DatabaseError