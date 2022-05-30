module Anonymous exposing (..)

import Component exposing (..)
import Credentials exposing (..)

-- elm/json
import Json.Decode exposing (..)

-- mdgriffith/elm-ui
import Element exposing (..)

-- MODEL

type alias UserId = String

type alias Token = String

type alias Model =
  { register : Credentials.Model
  , login    : Credentials.Model
  , registerSubmit : Submit UserId
  , loginSubmit : Submit Token
  }

init : () -> ( Model, Cmd Msg )
init _ =
  ( { register = emptyCredentials
    , login = emptyCredentials
    , registerSubmit = NotYetSubmitted
    , loginSubmit = NotYetSubmitted
    }
  , Cmd.none
  )

-- UPDATE

type Msg
  = RegisterData CredentialsMessage
  | LoginData CredentialsMessage
  | Register (SubmitMessage UserId)
  | Login (SubmitMessage Token)

updateModelWithRegisterSubmit : Model -> { model : Credentials.Model, submitState : Submit UserId } -> Model
updateModelWithRegisterSubmit model data = { model | registerSubmit = data.submitState, register = data.model }

updateModelWithLoginSubmit : Model -> { model : Credentials.Model, submitState : Submit Token } -> Model
updateModelWithLoginSubmit model data = { model | loginSubmit = data.submitState, login = data.model }

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

-- VIEW

view : Model -> Element Msg
view model = Component.mainRow
  "anonymous"
  [ Credentials.view "register" "Register User" RegisterData Register model.register
  , Credentials.view "login"    "Login"         LoginData    Login    model.login
  ]

-- HTTP

userIdDecoder : Decoder UserId
userIdDecoder = Json.Decode.string

tokenDecoder : Decoder Token
tokenDecoder = Json.Decode.string