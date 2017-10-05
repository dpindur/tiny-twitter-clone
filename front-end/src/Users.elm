module Users where

import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)
import Http
import Task
import Debug

import UsersTypes exposing (..)
import AppTypes as App

default : Model
default = Model []

init : (Model, Effects Action)
init =
    ( default
    , Effects.none
    )

update : Action -> () -> Model -> (Model, Effects Action)
update action _ model =
  case action of
    LoadedUsers Nothing ->
      (model, Effects.none)

    LoadedUsers (Just users) ->
      ({ model | users <- users }, Effects.none)

    FollowSuccess ->
      let log = Debug.log "follow" "success"
      in (model, Effects.none)

    FollowFailure ->
      let log = Debug.log "follow" "failure"
      in (model, Effects.none)


type alias Context =
  { actions : Signal.Address Action
  , parent : Signal.Address App.Action
  , token : Maybe String
  }

view : Context -> Model -> Html
view context model =
  div [ class "col s6 offset-s3" ] [
    div [ class "row" ] (List.map (viewUser context) model.users)
  ]

viewUser : Context -> User -> Html
viewUser context user =
  div [ class "col s4" ] [
    div [ class "card-panel teal lighten-1" ] [
      div [ class "row", noMarginBottom ] [
        div [ class "col s6" ] [
          i [ class "medium material-icons circle white" ] [ text "face" ]
        ],
        followBtn context user
      ],
      div [ class "row white-text", noMarginBottom ] [
        h5 [ textPadding ] [ text user.username ],
        p [ noMarginBottom, noMarginTop, textPadding] [ text "User blurb here..." ]
      ]
    ]
  ]

followBtn : Context -> User -> Html
followBtn context user =
  case context.token of
    Nothing ->
      div [ class "col s6" ] []
    Just token ->
      div [ class "col s6" ] [
        button [ class "right teal waves-effect waves-light btn"
               , btnFix
               , onClick context.parent (App.AttemptFollow user.username) ] [ text "Follow" ]
      ]

noMarginBottom : Attribute
noMarginBottom = style [ ("margin-bottom", "0px") ]

noMarginTop : Attribute
noMarginTop = style [ ("margin-top", "0px") ]

btnFix : Attribute
btnFix = style [ ("top", "22px"), ("padding", "0 1rem")]

textPadding : Attribute
textPadding = style [ ("padding-left", "1rem") ]

loadUsers : Effects Action
loadUsers =
  Http.send Http.defaultSettings
    { verb = "GET"
    , headers = []
    , url = usersUrl
    , body = Http.empty }
    |> Http.fromJson decodeUsers
    |> Task.toMaybe
    |> Task.map LoadedUsers
    |> Effects.task

decodeUsers : Decoder (List User)
decodeUsers =
  Json.Decode.list decodeUser

decodeUser : Decoder User
decodeUser =
  Json.Decode.object2 User
    ("userid" := Json.Decode.string)
    ("username" := Json.Decode.string)

usersUrl : String
usersUrl = "http://localhost:3000/users"

followUrl : String
followUrl = "http://localhost:3000/follow"

follow : Maybe String -> String -> Effects App.Action
follow token username =
  case token of
    Nothing    ->
      Effects.none

    Just token ->
      Http.send Http.defaultSettings
      { verb = "POST"
      , headers = [("X-Auth-Token", token)]
      , url = followUrl
      , body = Http.string <| "{ \"username\": \"" ++ username ++ "\"}" } -- Replace this with proper JSON encoding
      |> Task.toMaybe
      |> (\x -> Task.andThen x handleFollow)
      |> Effects.task

handleFollow : Maybe Http.Response -> Task.Task a App.Action
handleFollow result =
  case result of
    Just _  -> Task.succeed <| App.UsersAction FollowSuccess
    Nothing -> Task.succeed <| App.UsersAction FollowFailure