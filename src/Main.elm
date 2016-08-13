module Main exposing (..)

import Html exposing (Html, Attribute, a, div, hr, input, span, text)
import Html.App exposing (map)
import Navigation
import UrlParser exposing (Parser, (</>), format, int, oneOf, s, string)
import Pair
import Account


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
    = AccountPage
    | PairPage
    | UnknownPage


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
    , account : Account.Model
    , err : Maybe String
    }


init : Result String Page -> ( Model, Cmd Msg )
init pageResult =
    let
        initModel =
            { page = UnknownPage
            , account = Account.initModel
            , pair = Pair.initModel
            , err = Nothing
            }
    in
        case pageResult of
            Ok page ->
                case page of
                    PairPage ->
                        let
                            ( pairModel, pairCmd ) =
                                Pair.load
                        in
                            { initModel | pair = pairModel, page = PairPage } ! [ Cmd.map PairMsg pairCmd ]

                    _ ->
                        initModel ! []

            Err routeErr ->
                { initModel | err = Just routeErr } ! []



-- UPDATE


type Msg
    = AccountMsg Account.Msg
    | PairMsg Pair.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "DEBUG10" msg of
        PairMsg pairMsg ->
            let
                ( pairModel, cmd ) =
                    Pair.update pairMsg model.pair
            in
                { model | pair = pairModel } ! [ Cmd.map PairMsg cmd ]

        AccountMsg accountMsg ->
            let
                ( accountModel, cmd ) =
                    Account.update accountMsg model.account
            in
                { model | account = accountModel } ! [ Cmd.map AccountMsg cmd ]


content : Model -> Html Msg
content model =
    case model.page of
        PairPage ->
            map PairMsg (Pair.view model.pair)

        _ ->
            div [] [ text "Not implemented" ]


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
