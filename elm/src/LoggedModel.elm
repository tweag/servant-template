module LoggedModel exposing (..)

import Set exposing (..)


type alias Tag =
    String


type alias Content =
    { message : String
    , tags : Set Tag
    }
