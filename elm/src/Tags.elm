module Tags exposing (..)

import LoggedModel exposing (..)

-- MODEL

type alias Tags =
  { newTag : String
  , tags : List Tag
  }

init : Tags
init = Tags "" []

-- UPDATE

type Msg
  = NewTag String
  | Submit
