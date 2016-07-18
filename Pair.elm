module Pair exposing (..)

import Html exposing (Html, Attribute, a, div, hr, input, span, text)


-- MODEL


type alias Model =
    { totem : Maybe String
    }


init : ( Model, Cmd a )
init =
    ( { totem = Nothing }, Cmd.none )



-- UPDATE


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            model ! []


view : Model -> Html Msg
view model =
    div []
        [ text "Loading 1234..."
        ]
