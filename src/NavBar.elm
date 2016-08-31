module NavBar exposing (Page(..), view, Msg)

import Html exposing (Html, Attribute, a, div, hr, input, span, text, ul, li, nav)
import Html.Attributes exposing (href)
import Html.CssHelpers
import AdminCss


{ id, class, classList } =
    Html.CssHelpers.withNamespace "lamassuAdmin"



-- MODEL


type Page
    = AccountPage String
    | PairPage
    | CryptoConfigPage String String
    | ConfigPage String
    | UnknownPage


type alias Model =
    ()


initModel : Model
initModel =
    ()


load : ( Model, Cmd Msg )
load =
    ( (), Cmd.none )



-- UPDATE


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


activePage linkPage page =
    if (linkPage == page) then
        class [ AdminCss.NavBarItemActive ]
    else
        class []


view : Page -> Html Msg
view page =
    nav [ class [ AdminCss.NavBar ] ]
        [ div [] [ a [ activePage PairPage page, href "/pair" ] [ text "Pairing" ] ]
        , div [] [ a [ activePage (AccountPage "twilio") page, href "/account/twilio" ] [ text "Accounts" ] ]
        , div [] [ a [ activePage (CryptoConfigPage "commissions" "global") page, href "/config/commissions/global" ] [ text "Commissions" ] ]
        , div [] [ a [ activePage (ConfigPage "limits") page, href "/config/limits" ] [ text "Limits" ] ]
        ]
