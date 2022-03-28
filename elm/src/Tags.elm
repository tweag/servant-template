module Tags exposing (..)

import Helper exposing (..)
import LoggedModel exposing (..)

-- elm/html
import Html exposing (..)
import Html.Events exposing (..)

-- MODEL

type alias Model =
  { newTag : String
  , tags : List Tag
  }

init : Model
init = Model "" []

-- UPDATE

type Msg
  = NewTag String
  | Submit

update : ( List Tag -> Cmd msg ) -> Msg -> Model -> ( Model, Cmd msg )
update onSubmit msg model = case msg of
  NewTag newTag -> ( { model | newTag = newTag }, Cmd.none )
  Submit        ->
    let
      newTags = ( Tag model.newTag ) :: model.tags
    in
      ( { model | newTag = "", tags = newTags }, onSubmit newTags )

-- VIEW

view : ( Tag -> Html Msg ) -> String -> String -> String -> Model -> Html Msg
view viewTag header filterText submitText model = div []
  [ h3 [] [ text header ]
  , div []
    ( List.map viewTag model.tags )
  , viewInput "text" filterText model.newTag NewTag
  , button [ onClick Submit ] [ text submitText ]
  ]
