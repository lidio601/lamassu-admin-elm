module Pair exposing (..)

import Html exposing (Html, Attribute, a, div, hr, input, span, text)
import Html.Attributes exposing (id)
import Port


-- MODEL


type alias Model =
    { totem : Maybe String
    }


init : ( Model, Cmd a )
init =
    ( { totem = Nothing }, Port.qr "qr" (Debug.log "DEBUG1" "hellooo there") )



-- UPDATE


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "DEBUG0" msg of
        None ->
            model ! [ Port.qr "qr" (Debug.log "DEBUG1" "hello there") ]


view : Model -> Html Msg
view model =
    div []
        [ div [ id "qr" ] []
        ]
