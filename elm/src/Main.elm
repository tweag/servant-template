module Main exposing (..)

import Anonymous exposing (..)
import Credentials exposing (Submit(..))
import Credentials exposing (SubmitMessage(..))
import Logged exposing (..)

-- elm/browser
import Browser exposing (..)

-- elm/html
import Html exposing (..)
import Tuple exposing (mapBoth)

-- MAIN

main : Program () Model Msg
main = element
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

-- MODEL

type Model
  = Anonymous Anonymous.Model
  | LoggedIn Logged.Model

init : () -> ( Model, Cmd Msg )
init _ = Tuple.mapBoth Anonymous ( Cmd.map AnonymousMsg ) ( Anonymous.init () )

-- UPDATE

type Msg
  = AnonymousMsg Anonymous.Msg
  | LoggedInMsg Logged.Msg

updateAnonymous : Anonymous.Msg -> Anonymous.Model -> ( Model, Cmd Msg )
updateAnonymous msg anonymousModel = case msg of
  Login ( Succeeded token ) -> ( LoggedIn ( Logged.init token ), Cmd.map LoggedInMsg ( retrieveContents token [] ) )
  _                         -> Tuple.mapBoth Anonymous ( Cmd.map AnonymousMsg ) (Anonymous.update msg anonymousModel)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case ( msg, model ) of
  ( AnonymousMsg anonymousMsg, Anonymous anonymousModel ) -> updateAnonymous anonymousMsg anonymousModel
  ( LoggedInMsg loggedMsg    , LoggedIn logged )          -> mapBoth LoggedIn ( Cmd.map LoggedInMsg ) ( Logged.update loggedMsg logged )
  _                                                       -> ( model, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model = case model of
  Anonymous anonymousModel -> Html.map AnonymousMsg ( Anonymous.view anonymousModel )
  LoggedIn logged          -> Logged.view logged
