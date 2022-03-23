module Anonymous exposing (..)

import Credentials exposing (..)

-- elm/html
import Html exposing (..)

-- elm/json
import Json.Decode exposing (..)

-- MODEL

type alias UserId = String

type alias Token = String

type alias Model =
  { register : Credentials
  , login    : Credentials
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

-- VIEW

view : Model -> Html Msg
view model = div []
  [ div []
    [ h2 [] [ text "Register User" ]
    , credentialsForm RegisterData Register model.register
    ]
  , div []
    [ h2 [] [ text "Login" ]
    , credentialsForm LoginData Login model.login
    ]
  ]

-- HTTP

userIdDecoder : Decoder UserId
userIdDecoder = Json.Decode.string

tokenDecoder : Decoder Token
tokenDecoder = Json.Decode.string
