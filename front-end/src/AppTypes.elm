module AppTypes where

import Effects exposing (..)
import Html exposing (..)

import HomeTypes as Home
import LoginTypes as Login
import RegisterTypes as Register
import UsersTypes as Users

type alias Model =
  { username : String
  , password : String
  , token : Maybe String
  , page : Page
  }

type Page = HomePage Home.Model
          | LoginPage Login.Model
          | RegisterPage Register.Model
          | UsersPage Users.Model

type Action = HomeAction Home.Action
            | LoginAction Login.Action
            | RegisterAction Register.Action
            | UsersAction Users.Action
            | AttemptLogin String String
            | LoginSuccess String
            | AttemptRegister String String String
            | RegisterSuccess String
            | AttemptFollow String
            | ChangeToRegisterPage
            | ChangeToHomePage
            | ChangeToLoginPage
            | ChangeToUsersPage
            | ChangeToProfilePage
            | JQuery ()
            | Logout

type alias Component childAction parent context =
  { update : childAction -> parent -> context -> (parent, Effects childAction) }

type alias BuildComponent childAction parent child context =
    { parentTag : child -> parent
    , getChild : parent -> Maybe child
    , updateChild : childAction -> context -> child -> (child, Effects childAction)
    }

wireComponent : BuildComponent a b c d -> Component a b d
wireComponent args =
  let
    update action parent ctx =
      args.getChild parent                                   -- Extract Maybe child component
        |> Maybe.map (args.updateChild action ctx)           -- Update the child component
        |> Maybe.map (\x -> (args.parentTag (fst x), snd x)) -- Tag the child component with the parent
        |> Maybe.withDefault (parent, Effects.none)          -- Remove the maybe
  in
    { update = update }