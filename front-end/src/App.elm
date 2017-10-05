module App where

import Effects exposing (Effects, map, batch, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Task
import String exposing (isEmpty)

import Home
import Login
import Register
import Users
import Debug
import Interop exposing (..)

import AppTypes exposing (..)
import HomeTypes as Home
import LoginTypes as Login
import RegisterTypes as Register
import UsersTypes as Users

init : (Model, Effects Action)
init =
    ( Model "" "" Nothing (LoginPage Login.default)
    , Effects.none
    )

getHome : Page -> Maybe Home.Model
getHome page =
  case page of
    HomePage model -> Just model
    _              -> Nothing

getLogin : Page -> Maybe Login.Model
getLogin page =
  case page of
    LoginPage model -> Just model
    _               -> Nothing

getRegister : Page -> Maybe Register.Model
getRegister page =
  case page of
    RegisterPage model -> Just model
    _                  -> Nothing

getUsers : Page -> Maybe Users.Model
getUsers page =
  case page of
    UsersPage model -> Just model
    _               -> Nothing

home = wireComponent <| BuildComponent HomePage getHome Home.update
login = wireComponent <| BuildComponent LoginPage getLogin Login.update
register = wireComponent <| BuildComponent RegisterPage getRegister Register.update
users = wireComponent <| BuildComponent UsersPage getUsers Users.update

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    HomeAction subaction ->
      let
        (page', fx) = home.update subaction model.page model.token
      in
        ({ model | page <- page' }, map HomeAction fx)

    LoginAction subaction ->
      let
        (page', fx) = login.update subaction model.page model.token
      in
        ({ model | page <- page' }, map LoginAction fx)

    RegisterAction subaction ->
      let
        (page', fx) = register.update subaction model.page ()
      in
        ({ model | page <- page' }, map RegisterAction fx)

    UsersAction subaction ->
      let
        (page', fx) = users.update subaction model.page ()
      in
        ({ model | page <- page' }, map UsersAction fx)

    AttemptLogin username password ->
      ({ model | username <- username,
                 password <- password }, Login.attemptLogin username password)

    LoginSuccess token ->
      ({ model | token <- Just token,
                 page  <- HomePage Home.default }
      , Effects.batch [jQuery "CHARCOUNTER", jQuery "DROPDOWN", map HomeAction (Home.loadTweets (Just token) 0)])

    AttemptRegister username password passwordConfirm ->
      (model, Register.attemptRegister username password passwordConfirm)

    RegisterSuccess token ->
      ({ model | token <- Just token,
                 page  <- HomePage Home.default }
      , Effects.batch [jQuery "CHARCOUNTER", jQuery "DROPDOWN", map HomeAction (Home.loadTweets (Just token) 0)])

    AttemptFollow username ->
      let log = Debug.log "following" username
      in (model, Users.follow model.token username)

    ChangeToRegisterPage ->
      ({ model | page <- (RegisterPage Register.default) }, Effects.none)

    ChangeToLoginPage ->
      ({ model | page <- (LoginPage Login.default)}, Effects.none)

    ChangeToUsersPage ->
      ({ model | page <- (UsersPage Users.default)}, map UsersAction (Users.loadUsers))

    ChangeToHomePage ->
      case model.token of
        Nothing -> ({ model | page <- (LoginPage Login.default)}, Effects.none)
        Just token ->
          ( { model | page <- (HomePage Home.default)}
          , Effects.batch [jQuery "CHARCOUNTER", jQuery "DROPDOWN", map HomeAction (Home.loadTweets (Just token) 0)])

    Logout -> ({ model | username <- "",
                         password <- "",
                         token <- Nothing,
                         page <- (LoginPage Login.default) }
              , Effects.none)

    ChangeToProfilePage -> let log = Debug.log "test" "profile page"
                           in (model, Effects.none)

    JQuery _ -> (model, Effects.none)

    x -> let log = Debug.log "Unhandled pattern" x
         in (model, Effects.none)

view : Signal.Address Action -> Model -> Html
view address model =
  div [] [
    nav [] [ navbarView address model ],
    div [ class "row" ] [
      pageView address model
    ]
  ]

navbarView : Signal.Address Action -> Model -> Html
navbarView address model =
  case model.token of
    Just _  -> navbarLoggedIn address model.username
    Nothing -> navbarAnonymous address

navbarLoggedIn : Signal.Address Action -> String -> Html
navbarLoggedIn address username =
  div [ class "nav-wrapper teal lighten-1" ] [
    div [ class "row" ] [
      div [ class "col s5 offset-s1" ] [
        ul [ class "left" ] [
          li [] [ a [ href "#", onClick address ChangeToHomePage ] [ text "Home" ] ],
          li [] [ a [ href "#", onClick address ChangeToUsersPage ] [ text "Users" ] ],
          li [] [ a [ href "#" ] [ text "About" ] ]
        ]
      ],
      div [ class "col s3 offset-s1" ] [
        Html.form [] [
          div [ class "input-field" ] [
            input [ id "search", type' "search"] [],
            label [ for "search" ] [
              i [ class "material-icons" ] [ text "search" ]
            ]
          ]
        ]
      ],
      ul [ id "navDropdown", class "dropdown-content" ] [
        li [] [ a [ href "#", onClick address ChangeToProfilePage ] [ text "Profile" ] ],
        li [] [ a [ href "#", onClick address Logout ] [ text "Log Out" ] ]
      ],
      div [ class "col s1" ] [
        ul [ class "left" ] [
          li [] [
            a [ href "#"
              , class "dropdown-button"
              , attribute "data-activates" "navDropdown"
              ] [ text username
                , i [ class "material-icons right"] [ text "arrow_drop_down" ] ]
          ]
        ]
      ]
    ]
  ]

navbarAnonymous : Signal.Address Action -> Html
navbarAnonymous address =
  div [ class "nav-wrapper teal lighten-1" ] [
    div [ class "row" ] [
      div [ class "col s5 offset-s1" ] [
        ul [ class "left" ] [
          li [] [ a [ href "#", onClick address ChangeToHomePage ] [ text "Home" ] ],
          li [] [ a [ href "#", onClick address ChangeToUsersPage ] [ text "Users" ] ],
          li [] [ a [ href "#" ] [ text "About" ] ]
        ]
      ],
      div [ class "col s3 offset-s1" ] [
        Html.form [] [
          div [ class "input-field" ] [
            input [ id "search", type' "search"] [],
            label [ for "search" ] [
              i [ class "material-icons" ] [ text "search" ]
            ]
          ]
        ]
      ],
      div [ class "col s1" ] [
        ul [ class "left" ] [
          li [] [ a [ href "#", onClick address ChangeToLoginPage ] [ text "Login" ] ]
        ]
      ]
    ]
  ]

pageView : Signal.Address Action -> Model -> Html
pageView address model =
  case model.page of
    HomePage page ->
      Home.view (Signal.forwardTo address HomeAction) page

    LoginPage page ->
      let
        context = Login.Context (Signal.forwardTo address LoginAction) address
      in
        Login.view context page

    RegisterPage page ->
      let
        context = Register.Context (Signal.forwardTo address RegisterAction) address
      in
        Register.view context page

    UsersPage page ->
      let
        context = Users.Context (Signal.forwardTo address UsersAction) address model.token
      in
        Users.view context page

jQuery : String -> Effects Action
jQuery command =
  Signal.send interopMailbox.address command
    |> Task.map JQuery
    |> Effects.task