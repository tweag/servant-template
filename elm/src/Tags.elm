module Tags exposing (..)

import Helper exposing (..)
import LoggedModel exposing (..)
import Style exposing (..)

-- mgriffith/elm-ui
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

-- VIEW

removable : String -> Element Msg -> Element Msg
removable id element = row []
  [ element
  , Element.el [ onClick ( Remove id ) ] ( Element.text "x" )
  ]

viewRemovableTag : ( Tag -> Element Msg ) -> Tag -> Element Msg
viewRemovableTag viewTag tag = removable tag.name ( viewTag tag )

view : ( Tag -> Element Msg ) -> String -> String -> Model -> Element Msg
view viewTag label submitText model = column []
  [ Element.row [] ( List.map (viewRemovableTag viewTag) model.tags )
  , Element.el [] ( Element.Input.text []
    { onChange    = NewTag
    , text        = model.newTag
    , placeholder = Just ( placeholder [] ( Element.text label ) )
    , label       = labelAbove [] ( Element.text label )
    } )
  , Element.Input.button -- TODO factor out button
    [ Element.padding 5
    , Element.Background.color blue
    , Element.Border.color purple
    , Element.Border.width 2
    , Element.Border.rounded 10
    , Element.focused [ Element.Background.color purple ]
    ]
    { onPress = Just Submit
    , label   = Element.text "Submit"
    }
  ]
