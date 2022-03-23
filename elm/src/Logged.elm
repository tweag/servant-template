module Logged exposing (..)

import Anonymous exposing (Token)

-- elm/html
import Html exposing (..)

-- elm/http
import Http exposing (..)

-- elm/json
import Json.Decode exposing (..)

-- elm/url
import Url exposing (..)
import Url.Builder exposing (toQuery)

-- MODEL

type alias Tag =
  { name : String
  }

type alias Content =
  { message : String
  , tags    : List Tag
  }

type alias Model =
  { token : Token
  , contents : List Content
  }

init : Token -> Model
init token = Model token []

-- UPDATE

type Msg
  = FetchSuccessful (List Content)
  | FetchFailed Http.Error

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
  FetchSuccessful contents -> ( { model | contents = contents }, Cmd.none )
  FetchFailed _            -> ( model, Cmd.none )

-- VIEW

viewTag : Tag -> Html msg
viewTag tag = div [] [ text tag.name ]

viewContent : Content -> Html msg
viewContent content = tr []
  [ td [] [ text content.message ]
  , td [] ( List.map viewTag content.tags )
  ]

view : Model -> Html msg
view model = div []
  [ table []
    [ thead []
      [ tr []
        [ th [] [ text "content" ]
        , th [] [ text "tags" ]
        ]
      ]
    , tbody [] ( List.map viewContent model.contents )
    ]
  ]

-- HTTP

retrieveUrl : List Tag -> Url
retrieveUrl tags =
  { protocol = Http
  , host     = "localhost"
  , port_    = Just 8080
  , path     = "/get-contents"
  , query    = Just ( toQuery ( List.map ( \{name} -> Url.Builder.string "tag" name) tags ) )
  , fragment = Nothing
  }

retrieveContents : Token -> List Tag -> Cmd Msg
retrieveContents token tags = Http.request
  { method  = "GET"
  , headers = [ Http.header "Authorization" ( String.append "Bearer " token ) ]
  , url     = Url.toString ( retrieveUrl tags )
  , body    = emptyBody
  , expect  = expectJson handleContentsResponse ( list wrappedContentDecoder )
  , timeout = Nothing
  , tracker = Nothing
  }

handleContentsResponse : Result Http.Error ( List Content ) -> Msg
handleContentsResponse result = case result of
  Ok value  -> FetchSuccessful value
  Err error -> FetchFailed error

tagDecoder : Decoder Tag
tagDecoder = Json.Decode.map Tag
  ( field "name" string )

contentDecoder : Decoder Content
contentDecoder = map2 Content
  ( field "message" string )
  ( field "tags"    ( list ( Json.Decode.map Tag string )))

wrappedContentDecoder : Decoder Content
wrappedContentDecoder = Json.Decode.map identity
  ( field "content" contentDecoder )
