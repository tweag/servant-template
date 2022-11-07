module Logged exposing (..)

import Anonymous exposing (..)
import Component exposing (..)
import Element exposing (..)
import Element.Border exposing (..)
import Element.Input exposing (..)
import Http exposing (..)
import Json.Decode exposing (..)
import Json.Encode exposing (..)
import LoggedModel exposing (..)
import Set exposing (..)
import Style exposing (..)
import Tags exposing (..)
import Url exposing (..)
import Url.Builder exposing (..)



-- MODEL


type alias Model =
    { token : Token
    , contents : List Content
    , filters : Tags.Model
    , newContent : String
    , newTags : Tags.Model
    }


init : Token -> Model
init token =
    Model token [] Tags.init "" Tags.init



-- UPDATE


type Msg
    = FetchSuccessful (List Content)
    | FetchFailed Http.Error
    | NewContent String
    | NewFilter Tags.Msg
    | NewTag Tags.Msg
    | SubmitContent
    | SubmitSuccessful Content
    | SubmitFailed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchSuccessful contents ->
            ( { model | contents = contents }, Cmd.none )

        FetchFailed _ ->
            ( model, Cmd.none )

        NewContent newContent ->
            ( { model | newContent = newContent }, Cmd.none )

        NewFilter filterMsg ->
            Tuple.mapFirst (\filters -> { model | filters = filters }) (Tags.update (retrieveContents model.token) filterMsg model.filters)

        NewTag tagMsg ->
            Tuple.mapFirst (\newTags -> { model | newTags = newTags }) (Tags.update (always Cmd.none) tagMsg model.newTags)

        SubmitContent ->
            ( model, addContent model.token (Content model.newContent model.newTags.tags) )

        SubmitSuccessful content ->
            ( { model | contents = content :: model.contents }, Cmd.none )

        SubmitFailed ->
            ( model, Cmd.none )



-- VIEW


viewTag : Tag -> Element msg
viewTag tag =
    Element.el
        [ normalPadding
        , normalSpacing
        ]
        (Element.text tag)


view : Model -> Element Msg
view model =
    Component.mainRow
        [ Component.mainColumn
            [ Component.columnTitle "Contents"
            , Element.map NewFilter (Tags.view viewTag "Filter by tag" "Add filter" model.filters)
            , Element.table
                [ normalPadding
                ]
                { data = model.contents
                , columns =
                    [ { header = tableHeader "Content"
                      , width = fill
                      , view =
                            \content ->
                                Element.el
                                    (normalPadding :: tableRowStyle)
                                    (Element.text content.message)
                      }
                    , { header = tableHeader "Tags"
                      , width = fill
                      , view =
                            \content ->
                                Element.el
                                    tableRowStyle
                                    (row [] (List.map viewTag (toList content.tags)))
                      }
                    ]
                }
            ]
        , Component.mainColumn
            [ Component.columnTitle "Add content"
            , Element.Input.text []
                { onChange = NewContent
                , text = model.newContent
                , placeholder = Just (Element.Input.placeholder [] (Element.text "New content"))
                , label = labelAbove [] (Element.text "New content")
                }
            , Element.map NewTag (Tags.view viewTag "New tag" "Add tag" model.newTags)
            , Component.button SubmitContent "Add content"
            ]
        ]



-- HTTP


retrieveUrl : Set Tag -> String
retrieveUrl tags =
    Url.Builder.custom
        (CrossOrigin "http://localhost:8080")
        [ "get-contents" ]
        (List.map (\tag -> Url.Builder.string "tag" tag) (toList tags))
        Nothing


authorization : Token -> Header
authorization token =
    Http.header "Authorization" (String.append "Bearer " token)


retrieveContents : Token -> Set Tag -> Cmd Msg
retrieveContents token tags =
    Http.request
        { method = "GET"
        , headers = [ authorization token ]
        , url = retrieveUrl tags
        , body = emptyBody
        , expect = expectJson handleContentsResponse (Json.Decode.list wrappedContentDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


handleContentsResponse : Result Http.Error (List Content) -> Msg
handleContentsResponse result =
    case result of
        Ok value ->
            FetchSuccessful value

        Err error ->
            FetchFailed error


tagDecoder : Decoder Tag
tagDecoder =
    field "name" Json.Decode.string


contentDecoder : Decoder Content
contentDecoder =
    map2 Content
        (field "message" Json.Decode.string)
        (field "tags" (Json.Decode.map fromList (Json.Decode.list Json.Decode.string)))


wrappedContentDecoder : Decoder Content
wrappedContentDecoder =
    Json.Decode.map identity
        (field "content" contentDecoder)


addContent : Token -> Content -> Cmd Msg
addContent token content =
    Http.request
        { method = "POST"
        , headers = [ authorization token ]
        , url = "http://localhost:8080/add-content"
        , body = jsonBody (contentEncoder content)
        , expect = expectWhatever (handleNewContentResponse content)
        , timeout = Nothing
        , tracker = Nothing
        }


handleNewContentResponse : Content -> Result Http.Error () -> Msg
handleNewContentResponse content result =
    case result of
        Err _ ->
            SubmitFailed

        Ok () ->
            SubmitSuccessful content


tagEncoder : Tag -> Json.Encode.Value
tagEncoder tag =
    Json.Encode.string tag


contentEncoder : Content -> Json.Encode.Value
contentEncoder content =
    Json.Encode.object
        [ ( "message", Json.Encode.string content.message )
        , ( "tags", Json.Encode.list tagEncoder (toList content.tags) )
        ]
