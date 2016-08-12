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
    , display : String
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


type alias InitialRecord =
    { config : Config }


type alias Model =
    Maybe InitialRecord


get : Cmd Msg
get =
    Http.getString "http://localhost:8093/initial"
        |> RemoteData.asCmd
        |> Cmd.map Initial


init : ( Model, Cmd Msg )
init =
    ( Nothing, get )



-- Decoders


fieldTypeDecoder : Decoder FieldType
fieldTypeDecoder =
    customDecoder string fieldTypeMap


fieldTypeMap : String -> Result String FieldType
fieldTypeMap fieldTypeString =
    let
        fieldTypeMaybe =
            Dict.get fieldTypeString fieldTypeDict

        error =
            fieldTypeString ++ " is not a supported field type"
    in
        Result.fromMaybe error fieldTypeMaybe


fieldDecoder : Decoder Field
fieldDecoder =
    object5 Field
        ("name" := string)
        ("display" := string)
        ("type" := fieldTypeDecoder)
        ("secret" := bool)
        ("required" := bool)


pluginDecoder : Decoder Plugin
pluginDecoder =
    object3 Plugin
        ("name" := string)
        ("display" := string)
        ("fields" := list fieldDecoder)


globalMachineDecoder : Decoder GlobalMachine
globalMachineDecoder =
    object1 GlobalMachine
        ("plugins" := list pluginDecoder)


globalCryptoDecoder : Decoder GlobalCrypto
globalCryptoDecoder =
    object1 GlobalCrypto
        ("global" := globalMachineDecoder)


configDecoder : Decoder Config
configDecoder =
    object1 Config
        ("global" := globalCryptoDecoder)


initialRecordDecoder : Decoder InitialRecord
initialRecordDecoder =
    object1 InitialRecord
        ("config" := configDecoder)



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
