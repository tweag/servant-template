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
  | Remove String

update : ( List Tag -> Cmd msg ) -> Msg -> Model -> ( Model, Cmd msg )
update onSubmit msg model = case msg of
  NewTag newTag -> ( { model | newTag = newTag }, Cmd.none )
  Submit        ->
    let
      tags = ( Tag model.newTag ) :: model.tags
    in
      ( { model | newTag = "", tags = tags }, onSubmit tags )
  Remove id     ->
    let
      tags = removeTag id model.tags
    in
      ( { model | tags = tags }, onSubmit tags )

removeTag : String -> List Tag -> List Tag
removeTag id tags = remove ( Tag id ) tags

-- VIEW

removable : String -> Html Msg -> Html Msg
removable id html = div []
  [ html
  , div [ onClick ( Remove id ) ] [ text "x" ]
  ]

viewRemovableTag : ( Tag -> Html Msg ) -> Tag -> Html Msg
viewRemovableTag viewTag tag = removable tag.name ( viewTag tag )

view : ( Tag -> Html Msg ) -> String -> String -> String -> Model -> Html Msg
view viewTag header filterText submitText model = div []
  [ h3 [] [ text header ]
  , div []
    ( List.map ( viewRemovableTag viewTag ) model.tags )
  , viewInput "text" filterText model.newTag NewTag
  , button [ onClick Submit ] [ text submitText ]
  ]
