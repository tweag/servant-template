module Logged exposing (..)

import Anonymous exposing (Token)

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

-- UPDATE

type Msg
  = Fetch
  | FetchSuccessful (List Content)
  | FetchFailed Http.Error

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
  , headers = [ header "Authorization" ( String.append "Bearer " token ) ]
  , url     = Url.toString ( retrieveUrl tags )
  , body    = emptyBody
  , expect  = expectJson handleContentsResponse ( list contentDecoder )
  , timeout = Nothing
  , tracker = Nothing
  }

handleContentsResponse : Result Http.Error ( List Content ) -> Msg
handleContentsResponse result = case result of
  Ok value  -> FetchSuccessful value
  Err error -> FetchFailed error

tagDecoder : Decoder Tag
tagDecoder = map Tag
  ( field "name" string )

contentDecoder : Decoder Content
contentDecoder = map2 Content
  ( field "message" string )
  ( field "tags"    ( list tagDecoder ))