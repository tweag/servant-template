module Tags exposing (..)

import LoggedModel exposing (..)

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
