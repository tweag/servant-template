module Main exposing (..)

import Anonymous exposing (..)

-- elm/browser
import Browser exposing (..)

import Html exposing (..)

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
init _ = Tuple.mapFirst Anonymous (Anonymous.init ())

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case model of
  Anonymous anonymousModel -> Tuple.mapFirst Anonymous (Anonymous.update msg anonymousModel)
  LoggedIn token           -> ( LoggedIn token, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model = case model of
  Anonymous anonymousModel -> Anonymous.view anonymousModel
  LoggedIn _               -> div [] []
