module LoginTypes where

type alias Model =
  { username : String
  , password : String
  , error : Maybe String
  }

type Action = UsernameChange String
            | PasswordChange String
            | FailedLogin