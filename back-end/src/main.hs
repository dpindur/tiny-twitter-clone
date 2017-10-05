{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}

-- alias run=./dist/build/haskell-twitter/haskell-twitter

import Web.Scotty
import Web.Scotty.Internal.Types (ScottyT)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types (emptyObject)
import qualified Data.Text.Lazy as T
import GHC.Generics
import Network.HTTP.Types
import Network.HTTP.Types.Status
import Network.Wai.Middleware.Cors

import qualified Login as Login
import qualified UserTimeline as UserTimeline
import qualified HomeTimeline as HomeTimeline
import qualified Tweet as Tweet
import qualified Follow as Follow
import qualified User as User
import qualified NewUser as NewUser

type Action = ActionM ()

data ErrorResponse = ErrorResponse
  { code :: Int
  , message :: String
  } deriving (Show, Generic)
instance FromJSON ErrorResponse
instance ToJSON ErrorResponse

policy :: CorsResourcePolicy
policy = simpleCorsResourcePolicy
       { corsRequestHeaders = [hContentType, "X-Auth-Token"]}

main :: IO ()
main = scotty 3000 $ do
  middleware (cors (const (Just policy)))
  application

application :: ScottyT T.Text IO ()
application = do
  post "/login"              postLoginA
  get  "/user_timelines/:id" getUserTimelineA
  post "/tweets"             postTweetA
  get  "/home_timeline"      getHomeTimelineA
  post "/follow"             postFollowA
  post "/users"              postUserA
  get  "/users"              getUsersA
  get  "/followers/:name"    getFollowersA
  get  "/following/:name"    getFollowingA

postLoginA :: Action
postLoginA = do
  body    <- jsonData
  success <- liftIO $ Login.createLoginToken
    (Login.username body)
    (Login.password body)
  case success of
    Right token                  -> json token
    Left Login.UsernameNotFound  -> status badRequest400 >> json (ErrorResponse 400 "Username not found.")
    Left Login.PasswordNotFound  -> status badRequest400 >> json (ErrorResponse 500 "Password not found.")
    Left Login.IncorrectPassword -> status badRequest400 >> json (ErrorResponse 401 "Incorrect password.")
    Left Login.DatabaseError     -> status badRequest400 >> json (ErrorResponse 500 "Error conecting to database.")

getUserTimelineA :: Action
getUserTimelineA = do
  id      <- param "id" :: ActionM String
  success <- liftIO $ UserTimeline.getUserTimeline id
  case success of
    Right tweets                       -> json tweets
    Left UserTimeline.UsernameNotFound -> status badRequest400 >> json (ErrorResponse 400 "Username not found.")
    Left UserTimeline.DatabaseError    -> status badRequest400 >> json (ErrorResponse 500 "Error conecting to database.")

postTweetA :: Action
postTweetA = do
  body    <- jsonData
  token   <- header "X-Auth-Token"
  success <- liftIO $ Tweet.createTweet (Tweet.body body) (fromMaybe "" (fmap T.unpack token))
  case success of
    Right tweet               -> json tweet
    Left Tweet.TokenInvalid   -> status badRequest400 >> json (ErrorResponse 401 "Auth token invalid.")
    Left Tweet.UserIDNotFound -> status badRequest400 >> json (ErrorResponse 400 "User id not found.")
    Left Tweet.DatabaseError  -> status badRequest400 >> json (ErrorResponse 500 "Error conecting to database.")
    Left Tweet.TweetTooLong   -> status badRequest400 >> json (ErrorResponse 500 "Tweet too long.")

getHomeTimelineA :: Action
getHomeTimelineA = do
  token   <- header "X-Auth-Token"
  page    <- param "page" `rescue` (\_ -> return 0) :: ActionM Integer
  success <- liftIO $ HomeTimeline.getHomeTimeline (fromMaybe "" (fmap T.unpack token)) page
  case success of
    Right tweets                       -> json tweets
    Left HomeTimeline.UsernameNotFound -> status badRequest400 >> json (ErrorResponse 400 "Username not found.")
    Left HomeTimeline.TokenInvalid     -> status badRequest400 >> json (ErrorResponse 401 "Auth token invalid.")
    Left HomeTimeline.DatabaseError    -> status badRequest400 >> json (ErrorResponse 500 "Error conecting to database.")
    Left HomeTimeline.PagingError      -> status badRequest400 >> json (ErrorResponse 500 "Paging Error.")

postFollowA :: Action
postFollowA = do
  body    <- jsonData
  token   <- header "X-Auth-Token"
  success <- liftIO $ Follow.createFollow (Follow.username body) (fromMaybe "" (fmap T.unpack token))
  case success of
    Right _                      -> json emptyObject
    Left Follow.TokenInvalid     -> status badRequest400 >> json (ErrorResponse 401 "Auth token invalid.")
    Left Follow.UsernameNotFound -> status badRequest400 >> json (ErrorResponse 400 "Username not found.")
    Left Follow.DatabaseError    -> status badRequest400 >> json (ErrorResponse 500 "Error conecting to database.")

postUserA :: Action
postUserA = do
  body    <- jsonData
  success <- liftIO $ User.createUser (NewUser.username body) (NewUser.password body)
  case success of
    Right token                     -> json token
    Left User.UsernameAlreadyExists -> status badRequest400 >> json (ErrorResponse 400 "Username already exist")
    Left User.DatabaseError         -> status badRequest400 >> json (ErrorResponse 500 "Error conecting to database.")

getUsersA :: Action
getUsersA = do
  success <- liftIO $ User.getUsers
  case success of
    Right users             -> json users
    Left User.DatabaseError -> status badRequest400 >> json (ErrorResponse 500 "Error conecting to database.")

getFollowersA :: Action
getFollowersA = do
  name    <- param "name" :: ActionM String
  success <- liftIO $ Follow.getFollowers name
  case success of
    Right followers              -> json followers
    Left Follow.UsernameNotFound -> status badRequest400 >> json (ErrorResponse 400 "Username not found.")
    Left Follow.DatabaseError    -> status badRequest400 >> json (ErrorResponse 500 "Error conecting to database.")

getFollowingA :: Action
getFollowingA = do
  name    <- param "name" :: ActionM String
  success <- liftIO $ Follow.getFollowing name
  case success of
    Right following              -> json following
    Left Follow.UsernameNotFound -> status badRequest400 >> json (ErrorResponse 400 "Username not found.")
    Left Follow.DatabaseError    -> status badRequest400 >> json (ErrorResponse 500 "Error conecting to database.")