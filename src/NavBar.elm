module NavBar exposing (Page(..), view, update, Msg)

import Html exposing (Html, Attribute, a, div, hr, input, span, text, ul, li, nav)
import Html.Events exposing (onClick)
import Html.CssHelpers
import Css.Classes
import Navigation exposing (newUrl)
import VirtualDom
import String


{ id, class, classList } =
    Html.CssHelpers.withNamespace "lamassuAdmin"



-- MODEL


type Page
    = AccountPage String
    | PairPage
    | ConfigPage String (Maybe String)
    | UnknownPage


type Category
    = Account
    | Config


type alias Model =
    { category : Maybe Category }


initModel : Model
initModel =
    { category = Nothing }


load : ( Model, Cmd Msg )
load =
    ( { category = Nothing }, Cmd.none )



-- UPDATE


type Msg
    = NewPage Page
    | NewCategory Category Page


maybeUrl : String -> List (Maybe String) -> Cmd Msg
maybeUrl root maybeStrings =
    List.filterMap identity maybeStrings
        |> List.append [ root ]
        |> String.join "/"
        |> String.cons '/'
        |> newUrl


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPage page ->
            case page of
                PairPage ->
                    model ! [ newUrl "/pair" ]

                AccountPage account ->
                    model ! [ newUrl ("/account/" ++ account) ]

                ConfigPage configGroup maybeCrypto ->
                    model ! [ maybeUrl ("config/" ++ configGroup) [ maybeCrypto ] ]

                UnknownPage ->
                    Debug.crash "Need unknown page"

        NewCategory category page ->
            update (NewPage page) ({ model | category = Just category })



-- Bit hacky, but we have to match only first parameter of page


activePage : Page -> Page -> VirtualDom.Property a
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


categoryView : ( String, Category, Page ) -> Html Msg
categoryView link =
    let
        ( desc, category, linkPage ) =
            link
    in
        div [ onClick (NewCategory category linkPage) ] [ text desc ]


linkView : Page -> Link -> Html Msg
linkView currentPage link =
    let
        ( desc, linkPage ) =
            link
    in
        div [ onClick (NewPage linkPage), activePage linkPage currentPage ] [ text desc ]


linksView : Page -> ( String, Category, Page ) -> List Link -> Html Msg
linksView currentPage catLink links =
    -- only show links if model.category is supplied category, also highlight category
    div []
        [ categoryView catLink
        , div [] (List.map (linkView currentPage) links)
        ]


view : Page -> Html Msg
view page =
    let
        l =
            linkView page

        ll =
            linksView page
    in
        nav [ class [ Css.Classes.NavBar ] ]
            [ l ( "Pairing", PairPage )
            , ll ( "Accounts", Account, AccountPage "twilio" )
                [ ( "Twilio", AccountPage "twilio" )
                ]
            , ll ( "Configuration", Config, ConfigPage "commissions" Nothing )
                [ ( "Commissions", ConfigPage "commissions" Nothing )
                , ( "Limits", ConfigPage "limits" Nothing )
                , ( "Fiat", ConfigPage "fiat" Nothing )
                , ( "Crypto services", ConfigPage "crypto-services" Nothing )
                , ( "Languages", ConfigPage "languages" Nothing )
                ]
            ]
