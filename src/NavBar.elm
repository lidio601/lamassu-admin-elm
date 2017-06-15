module NavBar exposing (view, routeToUrl)

import Html exposing (Html, Attribute, a, div, hr, input, span, text, ul, li, nav)
import Html.Events exposing (onClick)
import Html.CssHelpers
import Css.Classes
import String
import CoreTypes
    exposing
        ( Msg(..)
        , Category(..)
        , Route(..)
        )


{ id, class, classList } =
    Html.CssHelpers.withNamespace "lamassuAdmin"


maybeUrl : String -> List (Maybe String) -> String
maybeUrl root maybeStrings =
    List.filterMap identity maybeStrings
        |> List.append [ root ]
        |> String.join "/"


routeToUrl : Route -> String
routeToUrl route =
    case route of
        PairRoute ->
            "/#pair"

        AccountRoute account ->
            "/#account/" ++ account

        ConfigRoute configGroup maybeCrypto ->
            maybeUrl ("/#config/" ++ configGroup) [ maybeCrypto ]

        MaintenanceRoute ->
            "/#maintenance/"

        TransactionRoute ->
            "/#transaction/"

        NotFoundRoute ->
            Debug.crash "Need unknown route"


linkClasses : Route -> Route -> Bool -> Attribute msg
linkClasses linkRoute route isValid =
    let
        validityClass =
            if isValid then
                []
            else
                [ Css.Classes.InvalidGroup ]

        active =
            case route of
                ConfigRoute config _ ->
                    linkRoute == ConfigRoute config Nothing

                _ ->
                    linkRoute == route
    in
        if (active) then
            class ([ Css.Classes.NavBarRoute, Css.Classes.Active ] ++ validityClass)
        else
            class ([ Css.Classes.NavBarRoute ] ++ validityClass)


type alias Link =
    ( String, Route, Bool )


activeCategory : Maybe Category -> Category -> Bool -> Attribute msg
activeCategory maybeCurrentCategory linkedCategory isValid =
    let
        validityClass =
            if isValid then
                []
            else
                [ Css.Classes.InvalidGroup ]
    in
        case maybeCurrentCategory of
            Nothing ->
                class ([ Css.Classes.NavBarCategory ] ++ validityClass)

            Just currentCategory ->
                if currentCategory == linkedCategory then
                    class ([ Css.Classes.NavBarCategory, Css.Classes.Active ] ++ validityClass)
                else
                    class ([ Css.Classes.NavBarCategory ] ++ validityClass)


categoryView : Maybe Category -> ( String, Category, Route, Bool ) -> Html Msg
categoryView currentCategory link =
    let
        ( desc, category, linkRoute, isValid ) =
            link
    in
        div
            [ onClick (NewUrl (routeToUrl linkRoute))
            , activeCategory currentCategory category isValid
            ]
            [ text desc ]


linkView : Maybe Category -> Route -> Maybe Category -> Link -> Html Msg
linkView maybeCategory currentRoute maybeLinkedCategory link =
    let
        ( desc, linkRoute, isValid ) =
            link
    in
        div [ onClick (NewUrl (routeToUrl linkRoute)), linkClasses linkRoute currentRoute isValid ] [ text desc ]


linksView : Maybe Category -> Route -> ( String, Category, Route, Bool ) -> List Link -> Html Msg
linksView maybeCurrentCategory currentRoute ( catDesc, cat, route, isValid ) links =
    if maybeCurrentCategory == (Just cat) then
        div [ class [ Css.Classes.NavBarCategoryContainer ] ]
            [ categoryView maybeCurrentCategory ( catDesc, cat, route, isValid )
            , div [] (List.map (linkView maybeCurrentCategory currentRoute (Just cat)) links)
            ]
    else
        div [ class [ Css.Classes.NavBarCategoryContainer ] ]
            [ categoryView maybeCurrentCategory ( catDesc, cat, route, isValid )
            ]


determineConfigCategory : String -> Maybe Category
determineConfigCategory configCode =
    if List.member configCode [ "definition", "setup", "cashOut", "commissions", "balanceAlerts", "compliance" ] then
        Just MachineSettingsCat
    else if List.member configCode [ "walletSettings", "notifications" ] then
        Just GlobalSettingsCat
    else
        Nothing


allClear : List String -> Category -> Bool
allClear invalidGroups cat =
    not <| List.any (\groupCode -> determineConfigCategory groupCode == Just cat) invalidGroups


determineCategory : Route -> Maybe Category
determineCategory route =
    case route of
        AccountRoute account ->
            Just AccountCat

        ConfigRoute config _ ->
            determineConfigCategory config

        _ ->
            Nothing


view : Route -> List String -> Html Msg
view route invalidGroups =
    let
        maybeCategory =
            determineCategory route

        l =
            linkView maybeCategory route Nothing

        ll =
            linksView maybeCategory route

        isValid group =
            not (List.member group invalidGroups)

        allClearMachine =
            allClear invalidGroups MachineSettingsCat

        allClearGlobal =
            allClear invalidGroups GlobalSettingsCat

        configLink code display =
            ( display, ConfigRoute code Nothing, isValid code )
    in
        nav [ class [ Css.Classes.NavBar ] ]
            [ l ( "Transactions", TransactionRoute, True )
            , l ( "Maintenance", MaintenanceRoute, True )
            , ll ( "Machine Settings", MachineSettingsCat, ConfigRoute "definition" Nothing, allClearMachine )
                [ configLink "definition" "Definition"
                , configLink "setup" "Setup"
                , configLink "cashOut" "Cash Out"
                , configLink "commissions" "Commissions"
                , configLink "balanceAlerts" "Balance Alerts"
                , configLink "compliance" "Compliance"
                ]
            , ll ( "Global Settings", GlobalSettingsCat, ConfigRoute "walletSettings " Nothing, allClearGlobal )
                [ configLink "walletSettings" "Wallet Settings"
                , configLink "notifications" "Notifications"
                ]
            , ll ( "Third Party Services", AccountCat, AccountRoute "bitgo", True )
                [ ( "BitGo", AccountRoute "bitgo", True )
                , ( "Bitstamp", AccountRoute "bitstamp", True )
                , ( "Blockcypher", AccountRoute "blockcypher", True )
                , ( "Twilio", AccountRoute "twilio", True )
                , ( "Mailjet", AccountRoute "mailjet", True )
                ]
            , l ( "+ Add Machine", PairRoute, True )
            ]
