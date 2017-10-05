module Register where

import Effects exposing (Effects, map, batch, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Json.Encode
import Task
import Debug
import String
import Maybe exposing (withDefault)

import RegisterTypes exposing (..)
import AppTypes as App

default : Model
default = Model "" "" "" Nothing

init : (Model, Effects Action)
init =
    ( default
    , Effects.none
    )

update : Action -> () -> Model -> (Model, Effects Action)
update action _ model =
  case action of
    UsernameChange s ->
      ({ model | username <- s }, Effects.none)

    PasswordChange s ->
      ({ model | password <- s }, Effects.none)

    PasswordConfirmChange s ->
      ({ model | passwordConfirm <- s }, Effects.none)

    RegisterError s ->
      ({ model | error <- Just s }, Effects.none)

type alias Context =
  { actions : Signal.Address Action
  , parent : Signal.Address App.Action
  }

view : Context -> Model -> Html
view context model =
  div [ class "col s4 offset-s4 z-depth-1 card-panel" ] [
    viewError model.error,
    div [ class "row" ] [
      div [ class "input-field col s12" ] [
        i [ class "material-icons prefix" ] [ text "account_circle" ],
        input [ id "username"
              , class "validate"
              , type' "text"
              , value model.username
              , on "input" targetValue (Signal.message context.actions << UsernameChange) ] [],
        label [ for "username" ] [ text "Username" ]
      ]
    ],
    div [ class "row" ] [
      div [ class "input-field col s12" ] [
        i [ class "material-icons prefix" ] [ text "lock" ],
        input [ id "password"
              , class "validate"
              , type' "password"
              , value model.password
              , on "input" targetValue (Signal.message context.actions << PasswordChange)] [],
        label [ for "password" ] [ text "Password" ]
      ]
    ],
    div [ class "row" ] [
      div [ class "input-field col s12" ] [
        i [ class "material-icons prefix" ] [ text "lock" ],
        input [ id "passwordConfirm"
              , class "validate"
              , type' "password"
              , value model.passwordConfirm
              , on "input" targetValue (Signal.message context.actions << PasswordConfirmChange)] [],
        label [ for "passwordConfirm" ] [ text "Confirm Password" ]
      ]
    ],
    div [ class "row" ] [
      div [ class "input-field col s12" ] [
        button [ class "btn waves-effect waves-light col s12 teal lighten-2"
               , onClick context.parent (App.AttemptRegister model.username model.password model.passwordConfirm) ] [ text "Register" ]
      ]
    ]
  ]

viewError : Maybe String -> Html
viewError error =
  case error of
    Nothing -> div [] []
    Just msg ->
      div [ class "row", style [("margin-bottom", "0px")] ] [
        div [ class "red-text col s12 valign-wrapper" ] [
          i [ class "material-icons small valign" ] [ text "error" ],
          p [ class "valign", style [("display", "inline-block"), ("padding-left", "1rem")]] [ text msg ]
        ]
      ]

attemptRegister : String -> String -> String -> Effects App.Action
attemptRegister username password passwordConfirm =
  if String.length username == 0 then
    App.RegisterAction (RegisterError "Please enter a username.")
      |> Task.succeed
      |> Effects.task
  else if password == passwordConfirm && String.length password > 0 then
    Http.post decodeToken registerUrl (Http.string (Json.Encode.encode 0 (registerJson username password)))
      |> Task.toMaybe
      |> (\x -> Task.andThen x handleRegister)
      |> Effects.task
  else
    App.RegisterAction (RegisterError "Passwords must match.")
      |> Task.succeed
      |> Effects.task

handleRegister : Maybe String -> Task.Task a App.Action
handleRegister result =
  case result of
    Just token -> Task.succeed <| App.RegisterSuccess token
    Nothing -> Task.succeed <| App.RegisterAction (RegisterError "Username already taken.")

decodeToken : Json.Decoder String
decodeToken =
  Json.at ["token"] Json.string

registerUrl : String
registerUrl = "http://localhost:3000/users"

registerJson : String -> String -> Json.Value
registerJson username password =
  Json.Encode.object
    [ ("username", Json.Encode.string username)
    , ("password", Json.Encode.string password)
    ]