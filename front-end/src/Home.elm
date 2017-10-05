module Home where

import Effects exposing (Effects, map, batch, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)
import Json.Encode exposing (string)
import String exposing (isEmpty)
import Date exposing (..)
import Http
import Task
import Debug

import HomeTypes exposing (..)

default : Model
default = Model [] "" (Just 0)

init : (Model, Effects Action)
init =
    ( default
    , Effects.none
    )

update : Action -> Maybe String -> Model -> (Model, Effects Action)
update action token model =
  case action of
    LoadedTweets (Just response) ->
      ({ model | tweets <- model.tweets ++ response.tweets,
                 nextPage <- response.nextpage }, Effects.none)

    TweetBodyChange body ->
      ({ model | newtweet <- body}, Effects.none)

    PostTweet tweet ->
      if String.isEmpty tweet || (String.length tweet > 150) then
        (model, Effects.none)
      else
        (model, postTweet tweet token)

    TweetSuccess (Just tweet) ->
      ({ model | tweets <- tweet :: model.tweets,
                 newtweet <- "" }, Effects.none)

    LoadPage page ->
      (model, loadTweets token page)

view : Signal.Address Action -> Model -> Html
view address model =
  div [ class "col s6 offset-s3" ] [
    ul [ class "collection" ] ((viewPostTweet address model) :: (List.map viewTweet model.tweets) ++ viewMoreButton address model)
  ]

viewMoreButton : Signal.Address Action -> Model -> List Html
viewMoreButton address model =
  case model.nextPage of
    Nothing -> []
    Just page ->
      [
        li [ class "collection-item valign-wrapper" ] [
          div [ class "input-field valign" ] [
            button [ class "btn waves-effect waves-light teal lighten-2"
                   , onClick address (LoadPage page ) ] [ text "More" ]
          ]
        ]
      ]

viewPostTweet : Signal.Address Action -> Model -> Html
viewPostTweet address model =
  li [ class "collection-item" ] [
    div [ class "input-field" ] [
      textarea [ id "newpost"
               , class "materialize-textarea"
               , attribute "length" "150"
               , Html.Attributes.value model.newtweet
               , on "input" targetValue (Signal.message address << TweetBodyChange) ] [],
      label [ for "newpost" ] [ text "New Post" ]
    ],
    div [ class "input-field" ] [
      button [ class "btn waves-effect waves-light teal lighten-2"
             , onClick address (PostTweet model.newtweet) ] [ text "Post" ]
    ]
  ]

viewTweet : Tweet -> Html
viewTweet tweet =
  li [ class "collection-item avatar" ] [
    i [ class "material-icons circle teal" ] [ text "face" ],
    span [ class "title" ] [
      b [] [ text tweet.username ]
    ],
    span [ class "blue-grey-text text-lighten-3 right" ] [ text <| viewDate <| fromTime (tweet.time*1000) ],
    p [ style [("word-wrap", "break-word")] ] [ text tweet.body ]
  ]

viewDate : Date -> String
viewDate d =
  viewTime (minute d) (hour d) ++
  " - " ++
  (day d |> toString) ++
  " " ++
  (month d |> toString) ++
  " " ++
  (year d |> toString)

viewTime : Int -> Int -> String
viewTime minutes hours =
  let
    showNumber n =
      if n < 10 then "0" ++ toString n else toString n

    amOrPm mins hours =
      if | (hours == 12 && minutes > 0) -> "PM"
         | hours > 12 -> "PM"
         | otherwise -> "AM"
  in
    if hours > 12 then
      showNumber (hours - 12) ++ ":" ++ showNumber minutes ++ amOrPm minutes hours
    else
      showNumber hours ++ ":" ++ showNumber minutes ++ amOrPm minutes hours



-- Change this to String -> Effects Action
-- Move maybe check out into App.elm
loadTweets : Maybe String -> Int -> Effects Action
loadTweets maybetoken page = case maybetoken of
  Nothing -> Effects.none
  Just token ->
    Http.send Http.defaultSettings
      { verb = "GET"
      , headers = [("X-Auth-Token", token)]
      , url = homeUrl page
      , body = Http.empty }
      |> Http.fromJson decodeTweets
      |> Task.toMaybe
      |> Task.map LoadedTweets
      |> Effects.task

decodeTweet : Decoder Tweet
decodeTweet =
  Json.Decode.object4 Tweet
    ("userid" := Json.Decode.string)
    ("username" := Json.Decode.string)
    ("time" := Json.Decode.float)
    ("body" := Json.Decode.string)

decodeTweets : Decoder TweetResponse
decodeTweets =
  Json.Decode.object2 TweetResponse
    ("tweets" := Json.Decode.list decodeTweet)
    ("nextPage" := nullOr Json.Decode.int)

homeUrl : Int -> String
homeUrl page = "http://localhost:3000/home_timeline?page=" ++ (toString page)

-- Change this to String -> String -> Effects Action
-- Move maybe check out into App.elm
-- Also fix fromJson decoding
postTweet : String -> Maybe String -> Effects Action
postTweet tweet maybetoken = case maybetoken of
    Nothing -> Effects.none
    Just token ->
      Http.send Http.defaultSettings
      { verb = "POST"
      , headers = [("X-Auth-Token", token)]
      , url = tweetUrl
      , body = Http.string <| "{ \"body\": \"" ++ tweet ++ "\"}" } -- Replace this with proper JSON encoding
      |> Http.fromJson decodeTweet
      |> Task.toMaybe
      |> Task.map TweetSuccess
      |> Effects.task

tweetUrl : String
tweetUrl = "http://localhost:3000/tweets"

nullOr : Decoder a -> Decoder (Maybe a)
nullOr decoder =
    Json.Decode.oneOf
    [ Json.Decode.null Nothing
    , Json.Decode.map Just decoder
    ]