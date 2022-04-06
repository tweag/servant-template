module Credentials exposing (..)

import Component exposing (..)
import Style exposing (..)

-- elm/html
import Html.Attributes exposing (class)

-- elm/http
import Http exposing (..)

-- elm/json
import Json.Encode exposing (..)
import Json.Decode exposing (..)

-- mdgriffith/elm-ui
import Element exposing (..)
import Element.Background exposing (..)
import Element.Border exposing (..)
import Element.Input exposing (..)

-- MODEL

type alias Model =
  { username : String
  , password : String
  }

emptyCredentials : Model
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

updateCredentials : CredentialsMessage -> Model -> Model
updateCredentials credentialsMessage credentials =
  case credentialsMessage of
    Username name     -> { credentials | username = name }
    Password password -> { credentials | password = password }

type SubmitMessage a
  = Submit
  | Failed Http.Error
  | Succeeded a

updateSubmit : Decoder a -> String -> Model -> SubmitMessage a -> Submit a -> ( { model : Model, submitState : Submit a }, Cmd (SubmitMessage a) )
updateSubmit decoder url credentials submitMessage submitState =
  case submitMessage of
    Submit          -> ( { model = emptyCredentials, submitState = submitState      }, submit decoder url credentials )
    Failed error    -> ( { model = credentials     , submitState = Failure error    }, Cmd.none )
    Succeeded value -> ( { model = credentials     , submitState = Successful value }, Cmd.none )

-- VIEW

view : String -> String -> (CredentialsMessage -> msg) -> (SubmitMessage a -> msg) -> Model -> Element msg
view identifier title liftModel liftMessage credentials = Component.mainColumn
  identifier
  [ Component.columnTitle title
  , column
    [ normalSpacing
    , Element.centerX
    ]
    [ Element.map liftModel ( column
      [ normalSpacing
      ]
      [ Element.Input.username
        [ htmlAttribute ( class "username" ) ]
        { onChange    = Username
        , text        = credentials.username
        , placeholder = Just ( Element.Input.placeholder [] ( Element.text "Username" ) )
        , label       = labelAbove [] ( Element.text "Username" )
        }
      , Element.Input.newPassword
        [ htmlAttribute ( class "password" ) ]
        { onChange    = Password
        , text        = credentials.password
        , placeholder = Just ( Element.Input.placeholder [] ( Element.text "Password" ) )
        , label       = labelAbove [] ( Element.text "Password" )
        , show        = False
        }
      ] )
    , Element.map liftMessage ( Component.button Submit "Submit" )
    ]
  ]

-- HTTP

submit : Decoder a -> String -> Model -> Cmd ( SubmitMessage a )
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
