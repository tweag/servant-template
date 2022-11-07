module Credentials exposing (..)

import Component exposing (..)
import Element exposing (..)
import Element.Background exposing (..)
import Element.Border exposing (..)
import Element.Input exposing (..)
import Http exposing (..)
import Json.Decode exposing (..)
import Json.Encode exposing (..)
import Style exposing (..)



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
        Username name ->
            { credentials | username = name }

        Password password ->
            { credentials | password = password }


type SubmitMessage a
    = Submit
    | Failed Http.Error
    | Succeeded a


updateSubmit : Decoder a -> String -> Model -> SubmitMessage a -> Submit a -> ( Submit a, Cmd (SubmitMessage a) )
updateSubmit decoder url credentials submitMessage model =
    case submitMessage of
        Submit ->
            ( model, submit decoder url credentials )

        Failed error ->
            ( Failure error, Cmd.none )

        Succeeded value ->
            ( Successful value, Cmd.none )



-- VIEW


view : String -> (CredentialsMessage -> msg) -> (SubmitMessage a -> msg) -> Model -> Element msg
view title liftModel liftMessage credentials =
    Component.mainColumn
        [ Component.columnTitle title
        , column
            [ normalSpacing
            , Element.centerX
            ]
            [ Element.map liftModel
                (column
                    [ normalSpacing
                    ]
                    [ Element.Input.username []
                        { onChange = Username
                        , text = credentials.username
                        , placeholder = Just (Element.Input.placeholder [] (Element.text "Username"))
                        , label = labelAbove [] (Element.text "Username")
                        }
                    , Element.Input.newPassword []
                        { onChange = Password
                        , text = credentials.password
                        , placeholder = Just (Element.Input.placeholder [] (Element.text "Password"))
                        , label = labelAbove [] (Element.text "Password")
                        , show = False
                        }
                    ]
                )
            , Element.map liftMessage (Component.button Submit "Submit")
            ]
        ]



-- HTTP


submit : Decoder a -> String -> Model -> Cmd (SubmitMessage a)
submit decoder url register =
    Http.post
        { url = url
        , body =
            jsonBody
                (Json.Encode.object
                    [ ( "username", Json.Encode.string register.username )
                    , ( "password", Json.Encode.string register.password )
                    ]
                )
        , expect = expectJson handleSubmitResponse decoder
        }


handleSubmitResponse : Result Http.Error a -> SubmitMessage a
handleSubmitResponse result =
    case result of
        Ok value ->
            Succeeded value

        Err error ->
            Failed error
