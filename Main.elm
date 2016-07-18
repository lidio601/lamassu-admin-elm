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
        [ format PairPage (s "" </> s "Main.elm")
        ]



-- MODEL


type alias Model =
    { page : Page
    , pair : Pair.Model
    }


init : Result String Page -> ( Model, Cmd Msg )
init result =
    let
        ( pairModel, _ ) =
            Pair.init
    in
        urlUpdate result { page = PairPage, pair = pairModel }



-- UPDATE


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            model ! []


content : Model -> Html Msg
content model =
    case model.page of
        PairPage ->
            map (\_ -> None) (Pair.view model.pair)


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
