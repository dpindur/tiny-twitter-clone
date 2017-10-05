module RegisterTypes where

type alias Model =
  { username : String
  , password : String
  , passwordConfirm : String
  , error : Maybe String
  }

type Action = UsernameChange String
            | PasswordChange String
            | PasswordConfirmChange String
            | RegisterError String