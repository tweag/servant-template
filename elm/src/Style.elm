module Style exposing (..)

import Element exposing (..)
import Element.Font exposing (..)

-- COLORS

blue : Color
blue = rgb255 230 230 250

purple : Color
purple = rgb255 200 200 250

-- FONT SIZES

titleFont : Attr decorative msg
titleFont = Element.Font.size 40

-- SPACING

normalSpacing : Attribute msg
normalSpacing = Element.spacing 10

bigSpacing : Attribute msg
bigSpacing = Element.spacing 20

-- PADDING

normalPadding : Attribute msg
normalPadding = padding 10

bigPadding : Attribute msg
bigPadding = padding 20
