module Main exposing (..)

import String
import Html exposing (Html, Attribute, a, div, hr, input, span, text)
import Html.App exposing (map)
import Navigation
import Hop
import Hop.Types exposing (Config, Address, Query)
import Pair
import Account
import Config
import NavBar exposing (..)
import UrlParser exposing (..)
import Result exposing (withDefault)
import Html.Attributes exposing (class)
import Navigation exposing (newUrl)
import CoreTypes exposing (Msg(..), Route(..), Category(..))


hopConfig : Hop.Types.Config
hopConfig =
    { hash = True
    , basePath = ""
    }


main : Program Never
main =
    Navigation.program urlParser
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }



-- URL PARSERS


urlParser : Navigation.Parser ( Route, Address )
urlParser =
    let
        -- A parse function takes the normalised path from Hop after taking
        -- in consideration the basePath and the hash.
        -- This function then returns a result.
        parse path =
            -- First we parse using UrlParser.parse.
            -- Then we return the parsed route or NotFoundRoute if the parsed failed.
            -- You can choose to return the parse return directly.
            path
                |> UrlParser.parse identity routes
                |> Result.withDefault NotFoundRoute

        resolver =
            -- Create a function that parses and formats the URL
            -- This function takes 2 arguments: The Hop Config and the parse function.
            Hop.makeResolver hopConfig parse
    in
        -- Create a Navigation URL parser
        Navigation.makeParser (.href >> resolver)


routes : Parser (Route -> a) a
routes =
    let
        nonEmptyStringParser : String -> Result String String
        nonEmptyStringParser str =
            if String.isEmpty str then
                Err "Empty string"
            else
                Ok str

        nonEmptyString : Parser (String -> a) a
        nonEmptyString =
            custom "NON_EMPTY_STRING" nonEmptyStringParser
    in
        oneOf
            [ format (\account -> AccountRoute account) (s "account" </> string)
            , format PairRoute (s "pair")
            , format (\config crypto -> ConfigRoute config (Just crypto)) (s "config" </> string </> nonEmptyString)
            , format (\config -> ConfigRoute config Nothing) (s "config" </> string)
            ]



-- MODEL


type alias Model =
    { route : Route
    , address : Address
    , category : Maybe Category
    , pair : Pair.Model
    , account : Account.Model
    , config : Config.Model
    , err : Maybe String
    }


init : ( Route, Address ) -> ( Model, Cmd Msg )
init ( route, address ) =
    let
        model =
            { route = route
            , address = address
            , category = Nothing
            , account = Account.init
            , pair = Pair.init
            , config = Config.init
            , err = Nothing
            }
    in
        urlUpdate ( route, address ) model



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
                { model | config = configModel } ! [ Cmd.map ConfigMsg cmd ]

        NewRoute maybeCategory route ->
            let
                _ =
                    Debug.log "DEBUG28" "x"

                path =
                    routeToUrl (Debug.log "DEBUG27" route)

                command =
                    Hop.outputFromPath hopConfig (Debug.log "DEBUG26" path)
                        |> Navigation.newUrl
            in
                { model | category = maybeCategory } ! [ command ]


content : Model -> Html Msg
content model =
    case Debug.log "DEBUG20" model.route of
        PairRoute ->
            map PairMsg (Pair.view model.pair)

        AccountRoute _ ->
            map AccountMsg (Account.view model.account)

        ConfigRoute _ _ ->
            map ConfigMsg (Config.view model.config)

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


urlUpdate : ( Route, Address ) -> Model -> ( Model, Cmd Msg )
urlUpdate ( route, address ) model =
    let
        pagedModel =
            { model | route = route }
    in
        case Debug.log "DEBUG25" route of
            PairRoute ->
                let
                    ( pairModel, cmd ) =
                        Pair.load
                in
                    { pagedModel | category = Nothing, pair = pairModel } ! [ Cmd.map PairMsg cmd ]

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

            NotFoundRoute ->
                Debug.crash "Need to create 404"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
