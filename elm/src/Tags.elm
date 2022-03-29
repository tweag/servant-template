module Tags exposing (..)

import Component exposing (..)
import LoggedModel exposing (..)
import Style exposing (..)

-- mdgriffith/elm-ui
import Element exposing (..)
import Element.Background exposing (..)
import Element.Border exposing (..)
import Element.Events exposing (..)
import Element.Input exposing (placeholder)
import Element.Input exposing (labelAbove)

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

remove : a -> List a -> List a
remove value list = case list of
  []               -> []
  ( head :: tail ) ->
    if   head == value
    then remove value tail
    else head :: remove value tail

-- VIEW

removable : String -> Element Msg -> Element Msg
removable id element = row
  [ normalSpacing ]
  [ element
  , Element.el
    ( ( onClick ( Remove id ) ) :: buttonStyle )
    ( Element.text "x" )
  ]

viewRemovableTag : ( Tag -> Element Msg ) -> Tag -> Element Msg
viewRemovableTag viewTag tag = removable tag.name ( viewTag tag )

view : ( Tag -> Element Msg ) -> String -> String -> Model -> Element Msg
view viewTag label submitText model = column
  [ normalSpacing
  , Element.centerX
  ]
  [ Element.el [] ( Element.Input.text []
    { onChange    = NewTag
    , text        = model.newTag
    , placeholder = Just ( placeholder [] ( Element.text label ) )
    , label       = labelAbove [] ( Element.text label )
    } )
  , Component.button Submit submitText
  , Element.row [ normalSpacing ] ( List.map (viewRemovableTag viewTag) model.tags )
  ]
