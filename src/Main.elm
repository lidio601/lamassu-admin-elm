module Main exposing (..)

import Html exposing (Html, Attribute, a, div, hr, input, span, text, map)
import Html.Attributes exposing (class)
import Navigation
import Pair
import Account
import Config
import Machine
import NavBar exposing (..)
import UrlParser exposing ((</>), s, string)
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


route : UrlParser.Parser (Route -> a) a
route =
    UrlParser.oneOf
        [ UrlParser.map AccountRoute (s "account" </> string)
        , UrlParser.map PairRoute (s "pair")
        , UrlParser.map (\config crypto -> ConfigRoute config (Just crypto)) (s "config" </> string </> string)
        , UrlParser.map (\config -> ConfigRoute config Nothing) (s "config" </> string)
        , UrlParser.map (MachineRoute MachineActions) (s "machine" </> s "actions")
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
    , category : Maybe Category
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
            , category = Nothing
            , account = Account.init
            , pair = Pair.init
            , config = Config.init
            , machine = Machine.init
            , accounts = []
            , err = Nothing
            }

        ( newModel, newCmd ) =
            update model

        -- TODO: updateUrl, which should be called when UrlChange msg happens
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

        -- TODO: need to set current category somewhere
        NewUrl location ->
            model ! [ Navigation.newUrl location ]


content : Model -> Html Msg
content model =
    case Debug.log "DEBUG20" model.route of
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
    div []
        [ div [ class "grid" ]
            [ div [ class "unit one-quarter no-gutters lamassuAdminMainLeft" ]
                [ NavBar.view model.category model.route ]
            , div [ class "unit three-quarters lamassuAdminMainRight" ]
                [ div [ class "lamassuAdminContent" ]
                    [ content model ]
                ]
            ]
        ]



{- urlUpdate : ( Route, Address ) -> Model -> ( Model, Cmd Msg )
   urlUpdate ( route, address ) model =
       let
           pagedModel =
               { model | route = route }
       in
           case Debug.log "DEBUG25" route of
               PairRoute ->
                   { pagedModel | category = Nothing, pair = Pair.init } ! []

               AccountRoute account ->
                   let
                       ( accountModel, cmd ) =
                           Account.load account
                   in
                       { pagedModel | category = Just AccountCat, account = accountModel } ! [ Cmd.map AccountMsg cmd ]

               ConfigRoute config maybeCryptoCodeString ->
                   let
                       ( configModel, cmd ) =
                           Config.load pagedModel.config config maybeCryptoCodeString
                   in
                       { pagedModel | category = Just ConfigCat, config = configModel } ! [ Cmd.map ConfigMsg cmd ]

               MachineRoute machineSubRoute ->
                   let
                       ( machineModel, cmd ) =
                           Machine.load
                   in
                       { pagedModel | category = Just MachineCat, machine = machineModel }
                           ! [ Cmd.map MachineMsg cmd ]

               NotFoundRoute ->
                   Debug.crash "Need to create 404"

-}
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
