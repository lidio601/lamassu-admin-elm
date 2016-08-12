module Main exposing (..)

import Html exposing (Html, Attribute, a, div, hr, input, span, text)
import Html.App exposing (map)
import Navigation
import UrlParser exposing (Parser, (</>), format, int, oneOf, s, string)
import Pair
import Start


main : Program Never
main =
    Navigation.program (Navigation.makeParser parser)
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }



-- URL PARSERS


type Page
    = StartPage
    | PairPage


parser : Navigation.Location -> Result String Page
parser location =
    UrlParser.parse identity pageParser location.pathname


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ format PairPage (s "")
        ]



-- MODEL


type alias Model =
    { page : Page
    , pair : Pair.Model
    , start : Start.Model
    }


init : Result String Page -> ( Model, Cmd Msg )
init result =
    let
        ( pairModel, pairCmd ) =
            Pair.init

        ( startModel, startCmd ) =
            Start.init

        model =
            { page = PairPage
            , pair = pairModel
            , start = startModel
            }
    in
        model ! [ Cmd.map StartMsg startCmd, Cmd.map PairMsg pairCmd ]



-- UPDATE


type Msg
    = StartMsg Start.Msg
    | PairMsg Pair.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "DEBUG10" msg of
        PairMsg pairMsg ->
            let
                ( _, cmd ) =
                    Pair.update pairMsg model.pair
            in
                model ! [ Cmd.map PairMsg cmd ]

        StartMsg startMsg ->
            let
                ( startModel, cmd ) =
                    Start.update startMsg model.start
            in
                { model | start = startModel } ! [ Cmd.map StartMsg cmd ]


content : Model -> Html Msg
content model =
    case model.page of
        PairPage ->
            map PairMsg (Pair.view model.pair)

        StartPage ->
            div [] []


view : Model -> Html Msg
view model =
    div [] [ content model ]


urlUpdate : Result String Page -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    case Debug.log "result" result of
        Err _ ->
            model ! []

        Ok page ->
            { model | page = page } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
