module Component exposing (..)

import Style exposing (..)

-- elm/html
import Html.Attributes exposing (class, id)

-- mdgriffith/elm-ui
import Element exposing (..)
import Element.Background exposing (..)
import Element.Border exposing (..)
import Element.Input exposing (..)
import Element.Font

mainRow : String -> List ( Element msg ) -> Element msg
mainRow identifier elements = row
  [ Element.width fill
  , htmlAttribute ( id identifier )
  ]
  elements

mainColumn : String -> List ( Element msg ) -> Element msg
mainColumn identifier elements = column
  [ normalPadding
  , bigSpacing
  , Element.width fill
  , alignTop
  , htmlAttribute ( id identifier )
  ]
  elements

columnTitle : String -> Element msg
columnTitle title = el [ Element.centerX ] ( Element.text title )

button : msg -> String -> Element msg
button message label = Element.Input.button
    ( [ Element.padding 5
      , Element.focused [ Element.Background.color purple ]
      , htmlAttribute ( class "button" )
      ] ++ buttonStyle )
    { onPress = Just message
    , label   = Element.text label
    }

tableHeader : String -> Element msg
tableHeader header = Element.el
  [ headerFont
  , Element.Font.center
  ]
  ( Element.text header)