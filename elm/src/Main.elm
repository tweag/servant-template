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

type alias Token = String

type alias Credentials =
  { username : String
  , password : String
  }

emptyCredentials : Credentials
emptyCredentials =
  { username = ""
  , password = ""
  }

type Submit a
  = NotYetSubmitted
  | Successful a
  | Failure Http.Error

type alias Model =
  { register : Credentials
  , login    : Credentials
  , registerSubmit : Submit UserId
  , loginSubmit : Submit Token
  }

init : () -> ( Model, Cmd Msg )
init _ = ( Model emptyCredentials emptyCredentials NotYetSubmitted NotYetSubmitted, Cmd.none )

-- UPDATE

type CredentialsMessage
  = Username String
  | Password String

type SubmitMessage a
  = Submit
  | Failed Http.Error
  | Succeeded a

type Msg
  = RegisterData CredentialsMessage
  | LoginData CredentialsMessage
  | Register (SubmitMessage UserId)
  | Login (SubmitMessage Token)

updateCredentials : CredentialsMessage -> Credentials -> Credentials
updateCredentials credentialsMessage credentials =
  case credentialsMessage of
    Username name     -> { credentials | username = name }
    Password password -> { credentials | password = password }

updateSubmit : Decoder a -> String -> Credentials -> SubmitMessage a -> Submit a -> ( Submit a, Cmd (SubmitMessage a) )
updateSubmit decoder url credentials submitMessage model =
  case submitMessage of
    Submit          -> ( model, submit decoder url credentials )
    Failed error    -> ( Failure error, Cmd.none )
    Succeeded value -> ( Successful value, Cmd.none )

updateModelWithRegisterSubmit : Model -> Submit UserId -> Model
updateModelWithRegisterSubmit model registerSubmit = { model | registerSubmit = registerSubmit }

updateModelWithLoginSubmit : Model -> Submit Token -> Model
updateModelWithLoginSubmit model loginSubmit = { model | loginSubmit = loginSubmit}

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    RegisterData credentialsMessage -> ( { model | register = updateCredentials credentialsMessage model.register }, Cmd.none )
    LoginData credentialsMessage    -> ( { model | login = updateCredentials credentialsMessage model.login }, Cmd.none )
    Register registerMessage        -> Tuple.mapBoth
      (updateModelWithRegisterSubmit model)
      (Cmd.map Register)
      (updateSubmit userIdDecoder "http://localhost:8080/register" model.register registerMessage model.registerSubmit)
    Login loginMessage              -> Tuple.mapBoth
      ( updateModelWithLoginSubmit model )
      (Cmd.map Login)
      (updateSubmit tokenDecoder "http://localhost:8080/login" model.login loginMessage model.loginSubmit)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model = div []
  [ h1 [] [ text "Tagger" ]
  , div []
    [ h2 [] [ text "Register User" ]
    , credentialsForm RegisterData Register model.register
    ]
  , div []
    [ h2 [] [ text "Login" ]
    , credentialsForm LoginData Login model.login
    ]
  ]

credentialsForm : (CredentialsMessage -> Msg) -> (SubmitMessage a -> Msg) -> Credentials -> Html Msg
credentialsForm liftModel liftMessage credentials = div []
  [ Html.map liftModel (div []
    [ viewInput "text"     "Username" credentials.username Username
    , viewInput "password" "Password" credentials.password Password
    ])
  , button [ onClick (liftMessage Submit) ] [ text "Submit" ]
  ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, Html.Attributes.value v, onInput toMsg ] []

-- HTTP

submit : Decoder a -> String -> Credentials -> Cmd ( SubmitMessage a )
submit decoder url register = Http.post
  { url = url
  , body = jsonBody ( Json.Encode.object
    [ ( "username", Json.Encode.string register.username )
    , ( "password", Json.Encode.string register.password )
    ])
  , expect = expectJson handleSubmitResponse decoder
  }

handleSubmitResponse : Result Http.Error a -> SubmitMessage a
handleSubmitResponse result = case result of
  Ok value  -> Succeeded value
  Err error -> Failed error

userIdDecoder : Decoder UserId
userIdDecoder = Json.Decode.string

tokenDecoder : Decoder Token
tokenDecoder = Json.Decode.string
