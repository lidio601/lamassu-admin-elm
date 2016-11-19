module Main exposing (..)

import Html exposing (Html, Attribute, a, div, hr, input, span, text, map)
import Html.Attributes exposing (class)
import Navigation
import Pair
import Account
import Config
import Machine
import NavBar exposing (..)
import UrlParser exposing ((</>), s, string, top, parseHash)
import Http
import HttpBuilder exposing (..)
import RemoteData
import Navigation exposing (newUrl, Location)
import CoreTypes exposing (Msg(..), Route(..), Category(..), MachineSubRoute(..))
import AccountsDecoder exposing (accountsDecoder)


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- URL PARSERS


parseRoute : UrlParser.Parser (Route -> a) a
parseRoute =
    UrlParser.oneOf
        [ UrlParser.map AccountRoute (s "account" </> string)
        , UrlParser.map PairRoute (s "pair")
        , UrlParser.map (\config crypto -> ConfigRoute config (Just crypto)) (s "config" </> string </> string)
        , UrlParser.map (\config -> ConfigRoute config Nothing) (s "config" </> string)
        , UrlParser.map (MachineRoute MachineActions) (s "machine" </> s "actions")
        , UrlParser.map PairRoute top
        ]


getAccounts : Cmd Msg
getAccounts =
    get ("/api/accounts")
        |> withExpect (Http.expectJson accountsDecoder)
        |> send RemoteData.fromResult
        |> Cmd.map (RemoteData.withDefault [])
        |> Cmd.map LoadAccounts



-- MODEL


type alias Model =
    { location : Location
    , pair : Pair.Model
    , account : Account.Model
    , config : Config.Model
    , machine : Machine.Model
    , accounts : List ( String, String )
    , err : Maybe String
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            { location = location
            , account = Account.init
            , pair = Pair.init
            , config = Config.init
            , machine = Machine.init
            , accounts = []
            , err = Nothing
            }

        ( newModel, newCmd ) =
            urlUpdate location model
    in
        newModel ! [ newCmd, getAccounts ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PairMsg pairMsg ->
            let
                ( pairModel, cmd ) =
                    Pair.update (Debug.log "DEBUG22" pairMsg) model.pair
            in
                { model | pair = pairModel } ! [ Cmd.map PairMsg cmd ]

        AccountMsg accountMsg ->
            let
                ( accountModel, cmd ) =
                    Account.update accountMsg model.account
            in
                { model | account = accountModel } ! [ Cmd.map AccountMsg cmd ]

        ConfigMsg configMsg ->
            let
                ( configModel, cmd ) =
                    Config.update configMsg model.config
            in
                { model | config = configModel } ! [ Cmd.map ConfigMsg cmd, getAccounts ]

        MachineMsg machineMsg ->
            let
                ( machineModel, cmd ) =
                    Machine.update machineMsg model.machine
            in
                { model | machine = machineModel } ! [ Cmd.map MachineMsg cmd ]

        LoadAccounts accounts ->
            { model | accounts = Debug.log "DEBUG55" accounts } ! []

        NewUrl url ->
            model ! [ Navigation.newUrl url ]

        UrlChange location ->
            urlUpdate location model


content : Model -> Route -> Html Msg
content model route =
    case route of
        PairRoute ->
            map PairMsg (Pair.view model.pair)

        AccountRoute _ ->
            map AccountMsg (Account.view model.account)

        ConfigRoute _ _ ->
            map ConfigMsg (Config.view model.config)

        MachineRoute _ ->
            map MachineMsg (Machine.view model.machine)

        NotFoundRoute ->
            div [] [ text ("No such route") ]


view : Model -> Html Msg
view model =
    let
        route =
            Maybe.withDefault NotFoundRoute (parseHash parseRoute model.location)
    in
        div []
            [ div [ class "grid" ]
                [ div [ class "unit one-quarter no-gutters lamassuAdminMainLeft" ]
                    [ NavBar.view route ]
                , div [ class "unit three-quarters lamassuAdminMainRight" ]
                    [ div [ class "lamassuAdminContent" ]
                        [ content model route ]
                    ]
                ]
            ]


urlUpdate : Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    let
        route =
            Maybe.withDefault NotFoundRoute (parseHash parseRoute location)
    in
        case route of
            PairRoute ->
                { model | location = location, pair = Pair.init } ! []

            AccountRoute account ->
                let
                    ( accountModel, cmd ) =
                        Account.load account
                in
                    { model | location = location, account = accountModel } ! [ Cmd.map AccountMsg cmd ]

            ConfigRoute config maybeCryptoCodeString ->
                let
                    ( configModel, cmd ) =
                        Config.load model.config config maybeCryptoCodeString
                in
                    { model | location = location, config = configModel } ! [ Cmd.map ConfigMsg cmd ]

            MachineRoute machineSubRoute ->
                let
                    ( machineModel, cmd ) =
                        Machine.load
                in
                    { model | location = location, machine = machineModel }
                        ! [ Cmd.map MachineMsg cmd ]

            NotFoundRoute ->
                { model | location = location } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
