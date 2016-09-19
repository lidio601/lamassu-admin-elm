module NavBar exposing (Page(..), Category(..), Config, view, pageToUrl)

import Html exposing (Html, Attribute, a, div, hr, input, span, text, ul, li, nav)
import Html.Events exposing (onClick)
import Html.CssHelpers
import Css.Classes
import String


{ id, class, classList } =
    Html.CssHelpers.withNamespace "lamassuAdmin"



-- MODEL


type Category
    = AccountCat
    | ConfigCat


type Page
    = AccountPage String
    | PairPage
    | ConfigPage String (Maybe String)
    | UnknownPage


type alias Config msg =
    { toNewPageMsg : Page -> msg
    , toNewCategoryMsg : Category -> Page -> msg
    }



-- UPDATE


maybeUrl : String -> List (Maybe String) -> String
maybeUrl root maybeStrings =
    List.filterMap identity maybeStrings
        |> List.append [ root ]
        |> String.join "/"
        |> String.cons '/'


pageToUrl : Page -> String
pageToUrl page =
    case page of
        PairPage ->
            "/pair"

        AccountPage account ->
            "/account/" ++ account

        ConfigPage configGroup maybeCrypto ->
            maybeUrl ("config/" ++ configGroup) [ maybeCrypto ]

        UnknownPage ->
            Debug.crash "Need unknown page"


activePage : Page -> Page -> Attribute msg
activePage linkPage page =
    let
        active =
            case page of
                PairPage ->
                    linkPage == page

                AccountPage _ ->
                    linkPage == page

                ConfigPage config _ ->
                    linkPage == ConfigPage config Nothing

                UnknownPage ->
                    Debug.crash "Need unknown page"
    in
        if (active) then
            class [ Css.Classes.Active ]
        else
            class []


type alias Link =
    ( String, Page )


newPage : Config msg -> Page -> msg
newPage c page =
    c.toNewPageMsg page


newCategory : Config msg -> Category -> Page -> msg
newCategory c category page =
    c.toNewCategoryMsg category page


activeCategory : Maybe Category -> Category -> Attribute msg
activeCategory maybeCurrentCategory linkedCategory =
    case maybeCurrentCategory of
        Nothing ->
            class []

        Just currentCategory ->
            if currentCategory == linkedCategory then
                class [ Css.Classes.Active ]
            else
                class []


categoryView : Config msg -> Maybe Category -> ( String, Category, Page ) -> Html msg
categoryView c currentCategory link =
    let
        ( desc, category, linkPage ) =
            link
    in
        div
            [ onClick (newCategory c category linkPage)
            , activeCategory currentCategory category
            ]
            [ text desc ]


linkView : Config msg -> Page -> Link -> Html msg
linkView c currentPage link =
    let
        ( desc, linkPage ) =
            link
    in
        div [ onClick (newPage c linkPage), activePage linkPage currentPage ] [ text desc ]


linksView : Config msg -> Maybe Category -> Page -> ( String, Category, Page ) -> List Link -> Html msg
linksView c currentCategory currentPage catLink links =
    -- only show links if model.category is supplied category, also highlight category
    div [ class [ Css.Classes.NavBarCategory ] ]
        [ categoryView c currentCategory catLink
        , div [] (List.map (linkView c currentPage) links)
        ]


view : Config msg -> Maybe Category -> Page -> Html msg
view c category page =
    let
        l =
            linkView c page

        ll =
            linksView c category page
    in
        nav [ class [ Css.Classes.NavBar ] ]
            [ l ( "Pairing", PairPage )
            , ll ( "Accounts", AccountCat, AccountPage "twilio" )
                [ ( "Twilio", AccountPage "twilio" )
                ]
            , ll ( "Configuration", ConfigCat, ConfigPage "commissions" Nothing )
                [ ( "Commissions", ConfigPage "commissions" Nothing )
                , ( "Limits", ConfigPage "limits" Nothing )
                , ( "Fiat", ConfigPage "fiat" Nothing )
                , ( "Crypto services", ConfigPage "crypto-services" Nothing )
                , ( "Languages", ConfigPage "languages" Nothing )
                ]
            ]
