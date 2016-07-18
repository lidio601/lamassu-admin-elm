module Main exposing (..)

import Html exposing (Html, Attribute, a, div, hr, input, span, text)
import Navigation
import UrlParser exposing (Parser, (</>), format, int, oneOf, s, string)


main : Program Never
main =
    Navigation.program (Navigation.makeParser parser)
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }



-- URL PARSERS


type Page
    = Home
    | Pair
    | GlobalConfig


parser : Navigation.Location -> Result String Page
parser location =
    UrlParser.parse identity pageParser location.pathname


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ format Home (s "home")
        , format Pair (s "pair")
        , format GlobalConfig (s "global-config")
        ]



-- MODEL


type alias Model =
    { page : Page
    }


init : Result String Page -> ( Model, Cmd Msg )
init result =
    urlUpdate result (Model Home)



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
    div [] []


urlUpdate : Result String Page -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    case Debug.log "result" result of
        Err _ ->
            ( model, Navigation.modifyUrl "/" )

        Ok page ->
            { page = page
            }
                ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
