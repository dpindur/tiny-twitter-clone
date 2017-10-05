module Login where

import Effects exposing (Effects, map, batch, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Json.Encode
import Task
import Debug
import Maybe exposing (withDefault)

import AppTypes as App
import LoginTypes exposing (..)

default : Model
default = Model "" "" Nothing

init : (Model, Effects Action)
init =
    ( default
    , Effects.none
    )

update : Action -> Maybe String -> Model -> (Model, Effects Action)
update action token model =
  case action of
    UsernameChange s ->
      ({ model | username <- s }, Effects.none)

    PasswordChange s ->
      ({ model | password <- s }, Effects.none)

    FailedLogin ->
      ({ model | error <- Just "Invalid username or password." }, Effects.none)

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

attemptLogin : String -> String -> Effects App.Action
attemptLogin username password =
  Http.post decodeToken loginUrl (Http.string (Json.Encode.encode 0 (loginJson username password)))
    |> Task.toMaybe
    |> (\x -> Task.andThen x handleLogin)
    |> Effects.task

handleLogin : Maybe String -> Task.Task a App.Action
handleLogin result =
  case result of
    Just token -> Task.succeed <| App.LoginSuccess token
    Nothing -> Task.succeed <| App.LoginAction FailedLogin

decodeToken : Json.Decoder String
decodeToken =
  Json.at ["token"] Json.string

loginUrl : String
loginUrl = "http://localhost:3000/login"

loginJson : String -> String -> Json.Value
loginJson username password =
  Json.Encode.object
    [ ("username", Json.Encode.string username)
    , ("password", Json.Encode.string password)
    ]

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
        button [ class "btn waves-effect waves-light col s12 teal lighten-2"
               , onClick context.parent (App.AttemptLogin model.username model.password) ] [ text "Login" ]
      ],
      div [ class "input-field col s12" ] [
        button [ class "btn waves-effect waves-light col s12 teal lighten-2"
               , onClick context.parent App.ChangeToRegisterPage ] [ text "Register" ]
      ]
    ]
  ]