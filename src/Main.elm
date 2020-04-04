module Main exposing (main)

import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as CBlock
import Bootstrap.Form.Input as Input
import Bootstrap.Utilities.Display as Display
import Bootstrap.Utilities.Size as Size
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error)
import Json.Decode as Json
import Task exposing (Task, andThen, sequence, succeed)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Story =
    { by : String
    -- descendants : Int
    , id : Int
    , kids : List Int
    , score : Int
    , time : Int
    , title : String
    , sType : String
    , url : String
    }


type alias Model =
    { stories : List Story
    , idList : List Int
    , dlStatus : DownloadStatus
    , maxStoriesNum : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] [] Loading 20, getStoryIdList topStoryIdListUrl )



-- UPDATE


type DownloadStatus
    = Failure
    | Loading
    | Success


type Msg
    = GotIdList (Result Http.Error (List Int))
    | GotStoryInfo (Result Http.Error Story)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotIdList result ->
            case result of
                Ok idList ->
                    let
                        c =
                            Cmd.batch (List.map (\id -> Http.get { url = getStoryInfoUrl (String.fromInt id), expect = Http.expectJson GotStoryInfo storyDecoder }) (List.take model.maxStoriesNum idList))
                    in
                    ( { model
                        | idList = idList
                        , dlStatus = Success
                        , stories = []
                      }
                    , c
                    )

                Err _ ->
                    ( { model | dlStatus = Failure }, Cmd.none )

        GotStoryInfo result ->
            case result of
                Ok story ->
                    ( { model
                        | stories = model.stories ++ [ story ]
                        , dlStatus = Success
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | dlStatus = Failure }, Cmd.none )


viewIdList : Model -> Html Msg
viewIdList model =
    case model.dlStatus of
        Failure ->
            div []
                [ text "Failed connect server." ]

        Loading ->
            text "Loading..."

        Success ->
            div []
                [ ul [] (List.map (\x -> li [] [ text (String.fromInt x) ]) model.idList) ]


viewHNCard : Story -> Html Msg
viewHNCard s =
    Card.config [ Card.attrs [ Size.w75 ] ]
        |> Card.header
            []
            [h3 [] [ a [ href s.url ] [ text s.title ] ]]
        |> Card.block
            []
            [ CBlock.titleH4 [] [ text ("score:" ++ (String.fromInt s.score))
            , text "  "
            , text ("by " ++ s.by)]
            ]
        |> Card.view


viewStories : Model -> Html Msg
viewStories model =
    case model.dlStatus of
        Failure ->
            div []
                [ text "Failed connect server." ]

        Loading ->
            text "Loading..."

        Success ->
            div []
                [ ul [] (List.map (\x -> viewHNCard x) model.stories) ]



-- viewStories : Model -> Html Msg
-- viewStories model ->
--             case model
-- HTTP


topStoryIdListUrl : String
topStoryIdListUrl =
    "https://hacker-news.firebaseio.com/v0/topstories.json"


getStoryInfoUrl : String -> String
getStoryInfoUrl id =
    "https://hacker-news.firebaseio.com/v0/item/" ++ id ++ ".json"


getStoryIdList : String -> Cmd Msg
getStoryIdList url =
    Http.get
        { url = url
        , expect = Http.expectJson GotIdList (Json.list Json.int)
        }


storyDecoder : Json.Decoder Story
storyDecoder =
    Json.map8 Story
        -- (Json.field "descendants" Json.int)
        (Json.field "by" Json.string)
        (Json.field "id" Json.int)
        (Json.oneOf
            [ Json.field "kids" (Json.list Json.int)
            , Json.succeed []
            ]
        )
        (Json.field "score" Json.int)
        (Json.field "time" Json.int)
        (Json.field "title" Json.string)
        (Json.field "type" Json.string)
        (Json.field "url" Json.string)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ CDN.stylesheet
        , viewStories model ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
