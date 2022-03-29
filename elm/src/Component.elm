module Component exposing (..)

import Style exposing (..)

-- mdgriffith/elm-ui
import Element exposing (..)
import Element.Background exposing (..)
import Element.Border exposing (..)
import Element.Input exposing (..)

button : msg -> String -> Element msg
button message label = Element.Input.button
    [ Element.padding 5
    , Element.Background.color blue
    , Element.Border.color purple
    , Element.Border.width 2
    , Element.Border.rounded 10
    , Element.focused [ Element.Background.color purple ]
    ]
    { onPress = Just message
    , label   = Element.text label
    }
