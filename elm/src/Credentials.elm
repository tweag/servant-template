module Credentials exposing (..)

import Helper exposing (..)

-- elm/html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- elm/http
import Http exposing (..)

-- elm/json
import Json.Encode exposing (..)
import Json.Decode exposing (..)

-- MODEL

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

-- UPDATE

type CredentialsMessage
  = Username String
  | Password String

updateCredentials : CredentialsMessage -> Credentials -> Credentials
updateCredentials credentialsMessage credentials =
  case credentialsMessage of
    Username name     -> { credentials | username = name }
    Password password -> { credentials | password = password }

type SubmitMessage a
  = Submit
  | Failed Http.Error
  | Succeeded a

updateSubmit : Decoder a -> String -> Credentials -> SubmitMessage a -> Submit a -> ( Submit a, Cmd (SubmitMessage a) )
updateSubmit decoder url credentials submitMessage model =
  case submitMessage of
    Submit          -> ( model, submit decoder url credentials )
    Failed error    -> ( Failure error, Cmd.none )
    Succeeded value -> ( Successful value, Cmd.none )

-- VIEW

credentialsForm : (CredentialsMessage -> msg) -> (SubmitMessage a -> msg) -> Credentials -> Html msg
credentialsForm liftModel liftMessage credentials = div []
  [ Html.map liftModel (div []
    [ viewInput "text"     "Username" credentials.username Username
    , viewInput "password" "Password" credentials.password Password
    ])
  , button [ onClick (liftMessage Submit) ] [ text "Submit" ]
  ]

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
