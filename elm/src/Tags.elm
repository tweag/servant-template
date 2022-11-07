module Tags exposing (..)

import Component exposing (..)
import LoggedModel exposing (..)
import Style exposing (..)

-- elm/core
import Set exposing (..)

-- elm/html
import Html.Attributes exposing (class, id)

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
  , tags : Set Tag
  }

init : Model
init = Model "" empty

-- UPDATE

type Msg
  = NewTag String
  | Submit
  | Remove String

update : ( Set Tag -> Cmd msg ) -> Msg -> Model -> ( Model, Cmd msg )
update onSubmit msg model = case msg of
  NewTag newTag -> ( { model | newTag = newTag }, Cmd.none )
  Submit        ->
    let
      tags = insert model.newTag model.tags
    in
      ( { model | newTag = "", tags = tags }, onSubmit tags )
  Remove id     ->
    let
      tags = remove id model.tags
    in
      ( { model | tags = tags }, onSubmit tags )

-- VIEW

removable : String -> Element Msg -> Element Msg
removable id element = row
  [ normalSpacing
  , htmlAttribute ( class "removable" ) ]
  [ element
  , Element.el
    ( [ onClick ( Remove id )
      , htmlAttribute ( class "remove" )
      ]
      ++ buttonStyle )
    ( Element.text "x" )
  ]

viewRemovableTag : ( Tag -> Element Msg ) -> Tag -> Element Msg
viewRemovableTag viewTag tag = removable tag ( viewTag tag )

view : ( Tag -> Element Msg ) -> String -> String -> String -> Model -> Element Msg
view viewTag label submitText identifier model = column
  [ normalSpacing
  , Element.centerX
  , htmlAttribute (id identifier)
  ]
  [ Element.el [] ( Element.Input.text []
    { onChange    = NewTag
    , text        = model.newTag
    , placeholder = Just ( placeholder [] ( Element.text label ) )
    , label       = labelAbove [] ( Element.text label )
    } )
  , Component.button Submit submitText
  , Element.row [ normalSpacing ] ( List.map (viewRemovableTag viewTag) ( toList model.tags ) )
  ]
