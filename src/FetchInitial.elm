module FetchInitial exposing (..)

import Http
import RemoteData exposing (RemoteData, WebData)
import Json.Decode exposing (..)
import Dict


type FieldType
    = FieldString


fieldTypeDict : Dict.Dict String FieldType
fieldTypeDict =
    Dict.fromList [ ( "string", FieldString ) ]


type alias Field =
    { name : String
    , fieldType : FieldType
    , secret : Bool
    , required : Bool
    }


type alias Plugin =
    { code : String
    , display : String
    , fields : List Field
    }


type alias GlobalMachine =
    { plugins : List Plugin }


type alias GlobalCrypto =
    { global : GlobalMachine
    }


type alias Config =
    { global : GlobalCrypto }


type alias Model =
    Maybe { config : Config }


get : Cmd Msg
get =
    Http.getString "http://localhost:8093/initial"
        |> RemoteData.asCmd
        |> Cmd.map Initial


init : ( Model, Cmd Msg )
init =
    ( Nothing, get )



-- Decoders


fieldDecoder : String -> Decoder FieldType
fieldDecoder fieldTypeString =
    let
        fieldTypeMaybe =
            Dict.get fieldTypeString fieldTypeDict
    in
        case fieldTypeMaybe of
            Just fieldType ->
                succeed fieldType

            Nothing ->
                fail (fieldTypeString ++ " is not a supported field type")



-- UPDATE


type Msg
    = Initial (WebData String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "DEBUG0" msg of
        Initial webdata ->
            case webdata of
                RemoteData.NotAsked ->
                    model ! []

                RemoteData.Loading ->
                    model ! []

                RemoteData.Failure _ ->
                    model ! []

                RemoteData.Success string ->
                    model ! []
