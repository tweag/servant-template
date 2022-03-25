module Logged exposing (..)

import Anonymous exposing (..)
import Helper exposing (..)
import LoggedModel exposing (..)
import Tags exposing (..)

-- elm/html
import Html exposing (..)
import Html.Events exposing (..)

-- elm/http
import Http exposing (..)

-- elm/json
import Json.Decode exposing (..)
import Json.Encode exposing (..)

-- elm/url
import Url exposing (..)
import Url.Builder exposing (..)

-- MODEL

type alias Model =
  { token : Token
  , contents : List Content
  , filters : Tags
  , newContent : String
  , newTags : Tags
  }

init : Token -> Model
init token = Model token [] Tags.init "" Tags.init

-- UPDATE

type Msg
  = FetchSuccessful (List Content)
  | FetchFailed Http.Error
  | NewContent String
  | NewFilter String
  | SubmitFilter
  | NewTag String
  | SubmitTag
  | SubmitContent
  | SubmitSuccessful Content
  | SubmitFailed

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
  FetchSuccessful contents -> ( { model | contents = contents }, Cmd.none )
  FetchFailed _            -> ( model, Cmd.none )
  NewContent newContent    -> ( { model | newContent = newContent }, Cmd.none )
  NewTag newTag            -> ( { model | newTags = Tags newTag model.newTags.tags }, Cmd.none )
  NewFilter newFilter      -> ( { model | filters = Tags newFilter model.filters.tags }, Cmd.none )
  SubmitFilter             ->
    let
      filters = ( Tag model.filters.newTag ) :: model.filters.tags
    in
      ( { model | filters = Tags "" filters }, retrieveContents model.token filters )
  SubmitTag                -> ( { model | newTags = Tags "" ( ( Tag model.newTags.newTag ) :: model.newTags.tags) }, Cmd.none )
  SubmitContent            -> ( model, addContent model.token ( Content model.newContent model.newTags.tags ) )
  SubmitSuccessful content -> ( { model | contents = content :: model.contents }, Cmd.none )
  SubmitFailed             -> ( model, Cmd.none )

-- VIEW

viewTag : Tag -> Html msg
viewTag tag = div [] [ text tag.name ]

viewContent : Content -> Html msg
viewContent content = tr []
  [ td [] [ text content.message ]
  , td [] ( List.map viewTag content.tags )
  ]

view : Model -> Html Msg
view model = div []
  [ h2 [] [ text "Contents" ]
  , div []
    [ h3 [] [ text "Filters" ]
    , div []
      ( List.map viewTag model.filters.tags )
    , viewInput "text" "Filter by tag" model.filters.newTag NewFilter
    , button [ onClick SubmitFilter ] [ text "Add filter" ]
    , table []
      [ thead []
        [ tr []
          [ th [] [ text "content" ]
          , th [] [ text "tags" ]
          ]
        ]
      , tbody [] ( List.map viewContent model.contents )
      ]
    ]
  , div []
    [ h2 [] [ text "Add content" ]
    , viewInput "text" "Content" model.newContent NewContent
    , div []
      [ h3 [] [ text "Tags" ]
      , div []
        ( List.map viewTag model.newTags.tags )
      , viewInput "text" "New tag" model.newTags.newTag NewTag
      , button [ onClick SubmitTag ] [ text "Add tag" ]
      ]
    , button [ onClick SubmitContent ] [ text "Add content" ]
    ]
  ]

-- HTTP

retrieveUrl : List Tag -> String
retrieveUrl tags = Url.Builder.custom
  ( CrossOrigin "http://localhost:8080" )
  [ "get-contents" ]
  ( List.map ( \tag -> Url.Builder.string "tag" tag.name ) tags )
  Nothing

authorization : Token -> Header
authorization token = Http.header "Authorization" ( String.append "Bearer " token )

retrieveContents : Token -> List Tag -> Cmd Msg
retrieveContents token tags = Http.request
  { method  = "GET"
  , headers = [ authorization token ]
  , url     = retrieveUrl tags
  , body    = emptyBody
  , expect  = expectJson handleContentsResponse ( Json.Decode.list wrappedContentDecoder )
  , timeout = Nothing
  , tracker = Nothing
  }

handleContentsResponse : Result Http.Error ( List Content ) -> Msg
handleContentsResponse result = case result of
  Ok value  -> FetchSuccessful value
  Err error -> FetchFailed error

tagDecoder : Decoder Tag
tagDecoder = Json.Decode.map Tag
  ( field "name" Json.Decode.string )

contentDecoder : Decoder Content
contentDecoder = map2 Content
  ( field "message" Json.Decode.string )
  ( field "tags"    ( Json.Decode.list ( Json.Decode.map Tag Json.Decode.string )))

wrappedContentDecoder : Decoder Content
wrappedContentDecoder = Json.Decode.map identity
  ( field "content" contentDecoder )

addContent : Token -> Content -> Cmd Msg
addContent token content = Http.request
  { method = "POST"
  , headers = [ authorization token ]
  , url = "http://localhost:8080/add-content"
  , body = jsonBody ( contentEncoder content )
  , expect = expectWhatever ( handleNewContentResponse content )
  , timeout = Nothing
  , tracker = Nothing
  }

handleNewContentResponse : Content -> Result Http.Error () -> Msg
handleNewContentResponse content result = case result of
  Err _ -> SubmitFailed
  Ok () -> SubmitSuccessful content

tagEncoder : Tag -> Json.Encode.Value
tagEncoder tag = Json.Encode.string tag.name

contentEncoder : Content -> Json.Encode.Value
contentEncoder content = Json.Encode.object
  [ ( "message", Json.Encode.string content.message )
  , ( "tags", Json.Encode.list tagEncoder content.tags)
  ]
