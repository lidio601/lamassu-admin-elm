module InitialRecord exposing (..)

import Json.Decode exposing (..)
import Dict


type FieldType
    = FieldString


fieldTypeDict : Dict.Dict String FieldType
fieldTypeDict =
    Dict.fromList [ ( "string", FieldString ) ]


type alias Field =
    { code : String
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
        ("code" := string)
        ("display" := string)
        ("type" := fieldTypeDecoder)
        ("secret" := bool)
        ("required" := bool)


pluginDecoder : Decoder Plugin
pluginDecoder =
    object3 Plugin
        ("code" := string)
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


decode : String -> Result String InitialRecord
decode string =
    decodeString initialRecordDecoder string
