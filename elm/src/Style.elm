module Style exposing (..)

import Element exposing (..)
import Element.Background exposing (..)
import Element.Border exposing (..)
import Element.Font exposing (..)



-- COLORS


blue : Color
blue =
    rgb255 230 230 250


purple : Color
purple =
    rgb255 200 200 250



-- FONT SIZES


titleFont : Attr decorative msg
titleFont =
    Element.Font.size 40


headerFont : Attr decorative msg
headerFont =
    Element.Font.size 25



-- SPACING


normalSpacing : Attribute msg
normalSpacing =
    Element.spacing 10


bigSpacing : Attribute msg
bigSpacing =
    Element.spacing 20



-- PADDING


normalPadding : Attribute msg
normalPadding =
    padding 10


bigPadding : Attribute msg
bigPadding =
    padding 20



-- BUTTON STYLE


buttonStyle : List (Attribute msg)
buttonStyle =
    [ Element.Background.color blue
    , Element.Border.color purple
    , Element.Border.width 2
    , Element.Border.rounded 10
    ]


tableRowStyle : List (Attribute msg)
tableRowStyle =
    [ Element.Border.solid
    , Element.Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
    , height fill
    ]
