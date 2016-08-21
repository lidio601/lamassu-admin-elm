module NavBar exposing (..)

import Html exposing (Html, Attribute, a, div, hr, input, span, text, ul, li)
import Html.Attributes exposing (href)


-- MODEL


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


view : Model -> Html Msg
view model =
    ul []
        [ li [] [ a [ href "/pair" ] [ text "Pairing" ] ]
        , li [] [ a [ href "/account/twilio" ] [ text "Accounts" ] ]
        , li [] [ a [ href "/config" ] [ text "Config" ] ]
        ]
