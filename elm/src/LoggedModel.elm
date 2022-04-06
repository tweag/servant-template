module LoggedModel exposing (..)

-- elm/core
import Set exposing (..)

type alias Tag = String

type alias Content =
  { message : String
  , tags    : Set Tag
  }
