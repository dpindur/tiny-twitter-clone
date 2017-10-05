module UsersTypes where

type alias Model =
  { users : List User
  }

type alias User =
  { userid : String
  , username : String
  }

type Action = LoadedUsers (Maybe (List User))
            | FollowSuccess
            | FollowFailure