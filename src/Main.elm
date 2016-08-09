module Main exposing (..)

import Html exposing (Html, Attribute, a, div, hr, input, span, text)
import Html.App exposing (map)
import Navigation
import UrlParser exposing (Parser, (</>), format, int, oneOf, s, string)
import Pair


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
    = PairPage


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
    }


init : Result String Page -> ( Model, Cmd Msg )
init result =
    let
        ( pairModel, cmd ) =
            Pair.init
    in
        { page = PairPage, pair = pairModel } ! [ Cmd.map PairMsg cmd ]



-- UPDATE


type Msg
    = PairMsg Pair.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "DEBUG10" msg of
        PairMsg pairMsg ->
            let
                ( _, cmd ) =
                    Pair.update pairMsg model.pair
            in
                model ! [ Cmd.map PairMsg cmd ]


content : Model -> Html Msg
content model =
    case model.page of
        PairPage ->
            map PairMsg (Pair.view model.pair)


view : Model -> Html Msg
view model =
    div [] [ content model ]


urlUpdate : Result String Page -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    case Debug.log "result" result of
        Err _ ->
            model ! []

        Ok page ->
            { model
                | page = page
            }
                ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
