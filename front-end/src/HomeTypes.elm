module HomeTypes where

type alias Model =
  { tweets : List Tweet
  , newtweet : String
  , nextPage : Maybe Int
  }

type alias Tweet =
  { userid: String
  , username : String
  , time : Float
  , body : String
  }

type alias TweetResponse =
  { tweets : List Tweet
  , nextpage : Maybe Int
  }

type Action = LoadedTweets (Maybe TweetResponse)
            | TweetBodyChange String
            | PostTweet String
            | LoadPage Int
            | TweetSuccess (Maybe Tweet)


