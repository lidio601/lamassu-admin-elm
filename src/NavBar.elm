module NavBar exposing (Page(..), view, update, Msg)

import Html exposing (Html, Attribute, a, div, hr, input, span, text, ul, li, nav)
import Html.Events exposing (onClick)
import Html.CssHelpers
import Css.Classes
import Navigation exposing (newUrl)
import VirtualDom


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


type Msg
    = NewPage Page


update : Msg -> Model -> ( Model, Cmd Msg )
update (NewPage page) model =
    case page of
        PairPage ->
            model ! [ newUrl "/pair" ]

        AccountPage account ->
            model ! [ newUrl ("/account/" ++ account) ]

        CryptoConfigPage configGroup crypto ->
            model ! [ newUrl ("/config/" ++ configGroup ++ "/" ++ crypto) ]

        ConfigPage configGroup ->
            model ! [ newUrl ("/config/" ++ configGroup) ]

        UnknownPage ->
            model ! []



-- Bit hacky, but we have to match only first parameter of page


activePage : Page -> Page -> VirtualDom.Property a
activePage linkPage page =
    let
        active =
            case page of
                CryptoConfigPage config _ ->
                    linkPage == CryptoConfigPage config "global"

                _ ->
                    linkPage == page
    in
        if (active) then
            class [ Css.Classes.Active ]
        else
            class []


view : Page -> Html Msg
view page =
    nav [ class [ Css.Classes.NavBar ] ]
        [ div [ onClick (NewPage PairPage), activePage PairPage page ] [ text "Pairing" ]
        , div [ onClick (NewPage (AccountPage "twilio")), activePage (AccountPage "twilio") page ] [ text "Accounts" ]
        , div [ onClick (NewPage (CryptoConfigPage "commissions" "global")), activePage (CryptoConfigPage "commissions" "global") page ] [ text "Commissions" ]
        , div [ onClick (NewPage (ConfigPage "limits")), activePage (ConfigPage "limits") page ] [ text "Limits" ]
        , div [ onClick (NewPage (ConfigPage "fiat")), activePage (ConfigPage "fiat") page ] [ text "Fiat" ]
        ]
