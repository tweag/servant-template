module Logged exposing (..)

-- elm/http
import Http exposing (..)
import Json.Decode exposing (..)

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

retrieveContents : Cmd Msg
retrieveContents = Http.get
  { url    = "http://localhost:8080/get-contents"
  , expect = expectJson handleContentsResponse ( list contentDecoder )
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