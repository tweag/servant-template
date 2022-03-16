module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Encode exposing (..)
import Json.Decode exposing (..)

-- MAIN

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias UserId = String

type alias Model =
  { username : String
  , password : String
  , userId   : Maybe UserId
  , error    : Maybe Http.Error
  }

init : () -> ( Model, Cmd Msg )
init _ = ( Model "" "" Nothing Nothing, Cmd.none )

-- UPDATE

type Msg
  = Username String
  | Password String
  | Submit
  | SubmitFailed Http.Error
  | SubmitSucceeded UserId

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Username name          -> ( { model | username = name }, Cmd.none )
    Password password      -> ( { model | password = password }, Cmd.none )
    Submit                 -> ( model, submit model )
    SubmitFailed error     -> ( { model | error = Just error }, Cmd.none )
    SubmitSucceeded userId -> ( { model | userId = Just userId }, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model = div []
  [ viewInput "text"     "Username" model.username Username
  , viewInput "password" "Password" model.password Password
  , button [ onClick Submit ] [ text "Submit" ]
  ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, Html.Attributes.value v, onInput toMsg ] []

-- HTTP

submit : Model -> Cmd Msg
submit model = Http.post
  { url = "http://localhost:8080/register"
  , body = jsonBody ( Json.Encode.object
    [ ( "username", Json.Encode.string model.username )
    , ( "password", Json.Encode.string model.password )
    ])
  , expect = expectJson handleSubmitResponse userIdDecoder
  }

handleSubmitResponse : Result Http.Error UserId -> Msg
handleSubmitResponse result = case result of
  Ok userId -> SubmitSucceeded userId
  Err error -> SubmitFailed error

userIdDecoder : Decoder UserId
userIdDecoder = Json.Decode.string
