module Main exposing (..)

import Anonymous exposing (..)
import Logged exposing (..)

-- elm/browser
import Browser exposing (..)

import Html exposing (..)
import Credentials exposing (Submit(..))
import Credentials exposing (SubmitMessage(..))

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
  | LoggedIn Token

init : () -> ( Model, Cmd Msg )
init _ = Tuple.mapBoth Anonymous ( Cmd.map AnonymousMsg ) ( Anonymous.init () )

-- UPDATE

type Msg
  = AnonymousMsg Anonymous.Msg
  | LoggedInMsg Logged.Msg

updateAnonymous : Anonymous.Msg -> Anonymous.Model -> ( Model, Cmd Msg )
updateAnonymous msg anonymousModel = case msg of
  Login ( Succeeded token ) -> ( LoggedIn token, Cmd.none )
  _                         -> Tuple.mapBoth Anonymous ( Cmd.map AnonymousMsg ) (Anonymous.update msg anonymousModel)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case ( msg, model ) of
  ( AnonymousMsg anonymousMsg, Anonymous anonymousModel ) -> updateAnonymous anonymousMsg anonymousModel
  ( LoggedInMsg _            , LoggedIn token )           -> ( LoggedIn token, Cmd.none )
  _                                                       -> ( model, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model = case model of
  Anonymous anonymousModel -> Html.map AnonymousMsg ( Anonymous.view anonymousModel )
  LoggedIn _               -> div [] []
