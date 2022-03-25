module LoggedModel exposing (..)

type alias Tag =
  { name : String
  }

type alias Content =
  { message : String
  , tags    : List Tag
  }
