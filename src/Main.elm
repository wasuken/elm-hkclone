module Main exposing (main)

import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as CBlock
import Bootstrap.Form.Input as Input
import Bootstrap.Navbar as Navbar
import Bootstrap.Pagination as Pagination
import Bootstrap.Utilities.Display as Display
import Bootstrap.Utilities.Size as Size
import Browser
import Date exposing (Date, Interval(..), Unit(..), diff, fromPosix, toRataDie)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error)
import Json.Decode as Json
import List.Extra exposing (..)
import Task exposing (Task, andThen, sequence, succeed)
import Time exposing (Month(..), Posix, millisToPosix, now, posixToMillis, utc)
import Time.Extra as Time


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


type StoryType
    = New
    | Ask
    | Top
    | Job
    | Show


type alias Model =
    { stories : List Story
    , idList : List (List Int)
    , dlStatus : DownloadStatus
    , maxStoriesNum : Int
    , nowDatePosix : Maybe Posix
    , storyType : StoryType
    , navbarState : Navbar.State
    , offset : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( navbarState, navCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( Model [] [ [] ] Loading 20 Nothing Top navbarState 0, Cmd.batch [ getStoryIdList (storyIdListUrl Top), setToday, navCmd ] )



-- UPDATE


type DownloadStatus
    = Failure
    | Loading
    | Success


type Msg
    = GotIdList (Result Http.Error (List Int))
    | GotStoryInfo (Result Http.Error Story)
    | ReceivePosixDate Posix
    | NavbarMsg Navbar.State
    | ChangeStoryType StoryType
    | PaginationMsg Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotIdList result ->
            case result of
                Ok idList ->
                    let
                        idList_ =
                            groupsOf model.maxStoriesNum idList

                        ids =
                            case List.head (List.drop model.offset idList_) of
                                Just lst ->
                                    lst

                                Nothing ->
                                    []

                        c =
                            Cmd.batch (storiesASync ids)
                    in
                    ( { model
                        | idList = idList_
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

        ReceivePosixDate today ->
            ( { model | nowDatePosix = Just today }
            , Cmd.none
            )

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        ChangeStoryType t ->
            let
                cmd =
                    getStoryIdList (storyIdListUrl t)
            in
            ( { model | storyType = t}, cmd )

        PaginationMsg index ->
            let
                ids =
                    case List.head (List.drop model.offset model.idList) of
                        Just lst ->
                            lst

                        Nothing ->
                            []

                cmd =
                    Cmd.batch (storiesASync ids)
            in
            ( { model | offset = index
                      , stories = [] }, cmd )


setToday : Cmd Msg
setToday =
    now |> Task.perform ReceivePosixDate


viewHNCard : Story -> Posix -> Html Msg
viewHNCard s now =
    Card.config [ Card.attrs [ Size.w75 ] ]
        |> Card.header
            []
            [ h3 [] [ a [ href s.url ] [ text s.title ] ] ]
        |> Card.block
            []
            [ CBlock.titleH4 []
                [ text ("score:" ++ String.fromInt s.score)
                , text "  "
                , text ("by " ++ s.by)
                , text "  "
                , text (formatHourDiff s.time now ++ " minute ago")
                ]
            ]
        |> Card.view


formatHourDiff : Int -> Posix -> String
formatHourDiff targetSec now =
    let
        nowSec =
            posixToMillis now // 1000

        df =
            (nowSec - targetSec) // (60 * 60)
    in
    String.fromInt df


viewStories : Model -> Html Msg
viewStories model =
    let
        now =
            case model.nowDatePosix of
                Just datePosix ->
                    datePosix

                Nothing ->
                    millisToPosix 0
    in
    case model.dlStatus of
        Failure ->
            div []
                [ text "Failed connect server." ]

        Loading ->
            text "Loading..."

        Success ->
            div []
                [ Navbar.config NavbarMsg
                    |> Navbar.withAnimation
                    |> Navbar.brand [ href "#" ] [ text "Brand" ]
                    |> Navbar.items
                        [ Navbar.itemLink [ href "#", onClick (ChangeStoryType Top) ] [ text "Top" ]
                        , Navbar.itemLink [ href "#", onClick (ChangeStoryType New) ] [ text "New" ]

                        -- , Navbar.itemLink [ href "#", onClick (ChangeStoryType Ask) ] [ text "Ask" ]
                        , Navbar.itemLink [ href "#", onClick (ChangeStoryType Job) ] [ text "Job" ]
                        , Navbar.itemLink [ href "#", onClick (ChangeStoryType Show) ] [ text "Show" ]
                        ]
                    |> Navbar.view model.navbarState
                , ul [] (List.map (\x -> viewHNCard x now) model.stories)
                , Pagination.defaultConfig
                    |> Pagination.ariaLabel "Pagination"
                    |> Pagination.large
                    |> Pagination.itemsList
                        { selectedMsg = PaginationMsg
                        , prevItem = Just <| Pagination.ListItem [] [ text "Previous" ]
                        , nextItem = Just <| Pagination.ListItem [] [ text "Next" ]
                        , activeIdx = model.offset
                        , data = model.stories
                        , itemFn = \idx _ -> Pagination.ListItem [] [ text <| String.fromInt (idx + 1) ]
                        , urlFn = \idx _ -> "#/pages/" ++ String.fromInt (idx + 1)
                        }
                    |> Pagination.view
                ]



-- HTTP


storiesASync : List Int -> List (Cmd Msg)
storiesASync ids =
    List.map (\id -> Http.get { url = getStoryInfoUrl (String.fromInt id), expect = Http.expectJson GotStoryInfo storyDecoder }) ids


createStoryIdListUrl : String -> String
createStoryIdListUrl name =
    "https://hacker-news.firebaseio.com/v0/" ++ name ++ ".json"


topStoryIdListUrl : String
topStoryIdListUrl =
    createStoryIdListUrl "topstories"


newStoryIdListUrl : String
newStoryIdListUrl =
    createStoryIdListUrl "newstories"


showStoryIdListUrl : String
showStoryIdListUrl =
    createStoryIdListUrl "showstories"


askStoryIdListUrl : String
askStoryIdListUrl =
    createStoryIdListUrl "askstories"


jobStoryIdListUrl : String
jobStoryIdListUrl =
    createStoryIdListUrl "jobstories"


getStoryInfoUrl : String -> String
getStoryInfoUrl id =
    "https://hacker-news.firebaseio.com/v0/item/" ++ id ++ ".json"


getStoryIdList : String -> Cmd Msg
getStoryIdList url =
    Http.get
        { url = url
        , expect = Http.expectJson GotIdList (Json.list Json.int)
        }


storyIdListUrl : StoryType -> String
storyIdListUrl t =
    case t of
        Job ->
            jobStoryIdListUrl

        Show ->
            showStoryIdListUrl

        Ask ->
            askStoryIdListUrl

        Top ->
            topStoryIdListUrl

        New ->
            newStoryIdListUrl


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
        , viewStories model
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
