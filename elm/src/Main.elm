module Main exposing (..)

import Anonymous exposing (..)
import Browser exposing (..)
import Credentials exposing (Submit(..), SubmitMessage(..))
import Element exposing (..)
import Logged exposing (..)
import Set exposing (..)
import Style exposing (..)
import Tuple exposing (mapBoth)



-- MAIN


main : Program () Model Msg
main =
    element
        { init = init
        , view = Element.layout [] << view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Model
    = Anonymous Anonymous.Model
    | LoggedIn Logged.Model


init : () -> ( Model, Cmd Msg )
init _ =
    Tuple.mapBoth Anonymous (Cmd.map AnonymousMsg) (Anonymous.init ())



-- UPDATE


type Msg
    = AnonymousMsg Anonymous.Msg
    | LoggedInMsg Logged.Msg


updateAnonymous : Anonymous.Msg -> Anonymous.Model -> ( Model, Cmd Msg )
updateAnonymous msg anonymousModel =
    case msg of
        Login (Succeeded token) ->
            ( LoggedIn (Logged.init token), Cmd.map LoggedInMsg (retrieveContents token empty) )

        _ ->
            Tuple.mapBoth Anonymous (Cmd.map AnonymousMsg) (Anonymous.update msg anonymousModel)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( AnonymousMsg anonymousMsg, Anonymous anonymousModel ) ->
            updateAnonymous anonymousMsg anonymousModel

        ( LoggedInMsg loggedMsg, LoggedIn logged ) ->
            mapBoth LoggedIn (Cmd.map LoggedInMsg) (Logged.update loggedMsg logged)

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Element Msg
view model =
    Element.column
        [ Element.width fill
        ]
        [ Element.el
            [ titleFont
            , bigPadding
            , centerX
            ]
            (Element.text "Tagger")
        , case model of
            Anonymous anonymousModel ->
                Element.map AnonymousMsg (Anonymous.view anonymousModel)

            LoggedIn logged ->
                Element.map LoggedInMsg (Logged.view logged)
        ]
