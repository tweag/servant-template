module Helper exposing (..)

-- elm/html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, Html.Attributes.value v, onInput toMsg ] []

remove : a -> List a -> List a
remove value list = case list of
  []               -> []
  ( head :: tail ) ->
    if   head == value
    then remove value tail
    else head :: remove value tail
