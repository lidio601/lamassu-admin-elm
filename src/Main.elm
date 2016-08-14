module Main exposing (..)

import Html exposing (Html, Attribute, a, div, hr, input, span, text)
import Html.App exposing (map)
import Html.Attributes exposing (class)
import Navigation
import Pair
import Account
import NavBar


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
    case Debug.log "DEBUG6" location.hash of
        "#account" ->
            Ok AccountPage

        "#pair" ->
            Ok PairPage

        _ ->
            Err "No such page"



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

                    AccountPage ->
                        let
                            ( accountModel, accountCmd ) =
                                Account.load "twilio"
                        in
                            { initModel | account = accountModel, page = AccountPage } ! [ Cmd.map AccountMsg accountCmd ]

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
                { model | pair = pairModel, page = PairPage } ! [ Cmd.map PairMsg cmd ]

        AccountMsg accountMsg ->
            let
                ( accountModel, cmd ) =
                    Account.update accountMsg model.account
            in
                { model | account = accountModel, page = AccountPage } ! [ Cmd.map AccountMsg cmd ]

        NavBarMsg navBarMsg ->
            ( model, Cmd.none )


content : Model -> Html Msg
content model =
    case Debug.log "DEBUG7" model.page of
        PairPage ->
            map PairMsg (Pair.view model.pair)

        AccountPage ->
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
urlUpdate result model =
    case Debug.log "result" result of
        Err updateErr ->
            { model | err = Just updateErr } ! []

        Ok page ->
            case page of
                PairPage ->
                    let
                        ( pairModel, cmd ) =
                            Pair.load
                    in
                        { model | pair = pairModel, page = PairPage } ! [ Cmd.map PairMsg cmd ]

                AccountPage ->
                    let
                        ( accountModel, cmd ) =
                            Account.load "twilio"
                    in
                        { model | account = accountModel, page = AccountPage } ! [ Cmd.map AccountMsg cmd ]

                UnknownPage ->
                    { model | err = Just "Unknown page" } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
