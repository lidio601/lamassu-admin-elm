module NavBar exposing (view, pageToUrl)

import Html exposing (Html, Attribute, a, div, hr, input, span, text, ul, li, nav)
import Html.Events exposing (onClick)
import Html.CssHelpers
import Css.Classes
import String
import CoreTypes exposing (Msg(..), Category(..), Page(..))


{ id, class, classList } =
    Html.CssHelpers.withNamespace "lamassuAdmin"
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
            class [ Css.Classes.NavBarPage, Css.Classes.Active ]
        else
            class [ Css.Classes.NavBarPage ]


type alias Link =
    ( String, Page )


activeCategory : Maybe Category -> Category -> Attribute msg
activeCategory maybeCurrentCategory linkedCategory =
    case maybeCurrentCategory of
        Nothing ->
            class [ Css.Classes.NavBarCategory ]

        Just currentCategory ->
            if currentCategory == linkedCategory then
                class [ Css.Classes.NavBarCategory, Css.Classes.Active ]
            else
                class [ Css.Classes.NavBarCategory ]


categoryView : Maybe Category -> ( String, Category, Page ) -> Html Msg
categoryView currentCategory link =
    let
        ( desc, category, linkPage ) =
            link
    in
        div
            [ onClick (NewPage (Just category) linkPage)
            , activeCategory currentCategory category
            ]
            [ text desc ]


linkView : Maybe Category -> Page -> Maybe Category -> Link -> Html Msg
linkView maybeCategory currentPage maybeLinkedCategory link =
    let
        ( desc, linkPage ) =
            link
    in
        div [ onClick (NewPage maybeLinkedCategory linkPage), activePage linkPage currentPage ] [ text desc ]


linksView : Maybe Category -> Page -> ( String, Category, Page ) -> List Link -> Html Msg
linksView maybeCurrentCategory currentPage ( catDesc, cat, page ) links =
    if maybeCurrentCategory == (Just cat) then
        div [ class [ Css.Classes.NavBarCategoryContainer ] ]
            [ categoryView maybeCurrentCategory ( catDesc, cat, page )
            , div [] (List.map (linkView maybeCurrentCategory currentPage (Just cat)) links)
            ]
    else
        div [ class [ Css.Classes.NavBarCategoryContainer ] ]
            [ categoryView maybeCurrentCategory ( catDesc, cat, page )
            ]


view : Maybe Category -> Page -> Html Msg
view maybeCategory page =
    let
        l =
            linkView maybeCategory page Nothing

        ll =
            linksView maybeCategory page
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
