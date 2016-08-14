module Main exposing (..)

import Html exposing (Html, Attribute, a, div, hr, input, span, text)
import Html.App exposing (map)
import Html.Attributes exposing (class)
import Navigation
import Pair
import Account
import NavBar
import UrlParser exposing (..)
import Result exposing (withDefault)
import String


main : Program Never
main =
    Navigation.program (Navigation.makeParser pageParser)
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }



-- URL PARSERS


pageParser : Navigation.Location -> Result String Page
pageParser location =
    UrlParser.parse identity desiredPage (String.dropLeft 1 location.pathname)


type Page
    = AccountPage String
    | PairPage
    | UnknownPage


desiredPage : Parser (Page -> a) a
desiredPage =
    oneOf
        [ format AccountPage (s "account" </> string)
        , format PairPage (s "pair")
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
            { page = withDefault UnknownPage (Debug.log "DEBUG11" pageResult)
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
                            { initModel | pair = pairModel } ! [ Cmd.map PairMsg pairCmd ]

                    AccountPage account ->
                        let
                            ( accountModel, accountCmd ) =
                                Account.load (Debug.log "DEBUG14" account)
                        in
                            { initModel | account = accountModel } ! [ Cmd.map AccountMsg accountCmd ]

                    UnknownPage ->
                        initModel ! [ Cmd.none ]

            Err routeErr ->
                { initModel | err = Just routeErr } ! []



-- UPDATE


type Msg
    = AccountMsg Account.Msg
    | PairMsg Pair.Msg
    | NavBarMsg NavBar.Msg


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

        NavBarMsg navBarMsg ->
            ( model, Cmd.none )


content : Model -> Html Msg
content model =
    case Debug.log "DEBUG7" model.page of
        PairPage ->
            map PairMsg (Pair.view model.pair)

        AccountPage _ ->
            map AccountMsg (Account.view model.account)

        UnknownPage ->
            div [] [ text ("No such page") ]


view : Model -> Html Msg
view model =
    div []
        [ map NavBarMsg (NavBar.view ())
        , div [ class "nav" ] [ content model ]
        ]


urlUpdate : Result String Page -> Model -> ( Model, Cmd Msg )
urlUpdate pageResult model =
    let
        pagedModel =
            { model | page = withDefault UnknownPage pageResult }
    in
        case Debug.log "result" pageResult of
            Err updateErr ->
                { model | err = Just updateErr } ! []

            Ok page ->
                case page of
                    PairPage ->
                        let
                            ( pairModel, cmd ) =
                                Pair.load
                        in
                            { pagedModel | pair = pairModel } ! [ Cmd.map PairMsg cmd ]

                    AccountPage account ->
                        let
                            ( accountModel, cmd ) =
                                Account.load (Debug.log "DEBUG13" account)
                        in
                            { pagedModel | account = accountModel } ! [ Cmd.map AccountMsg cmd ]

                    UnknownPage ->
                        { model | err = Just "Unknown page" } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
