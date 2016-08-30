module ConfigDecoder exposing (..)

import Json.Decode exposing (..)
import FieldSetTypes exposing (FieldStatus)
import FieldSetDecoder exposing (fieldStatusDecoder, fieldStatusValueDecoder)
import ConfigTypes exposing (..)


nullOr : Decoder a -> Decoder (Maybe a)
nullOr decoder =
    oneOf
        [ null Nothing
        , map Just decoder
        ]


fieldValueTypeDecoder : String -> Decoder FieldValue
fieldValueTypeDecoder fieldType =
    case fieldType of
        "string" ->
            map FieldStringValue ("value" := string)

        "percentage" ->
            map FieldPercentageValue ("value" := float)

        "integer" ->
            map FieldIntegerValue ("value" := int)

        _ ->
            fail ("Unsupported field type: " ++ fieldType)


fieldValueDecoder : Decoder FieldValue
fieldValueDecoder =
    ("fieldType" := string) `andThen` fieldValueTypeDecoder


fieldStatusDecoder : Decoder FieldStatus
fieldStatusDecoder =
    ("code" := string) `andThen` fieldStatusValueDecoder


machineFieldDecoder : Decoder MachineField
machineFieldDecoder =
    object2 MachineField
        ("machine" := machineDecoder)
        ("value" := fieldValueDecoder)


cryptoFieldDecoder : Decoder CryptoField
cryptoFieldDecoder =
    object2 CryptoField
        ("crypto" := cryptoDecoder)
        ("value" := fieldValueDecoder)


cryptoMachineFieldDecoder : Decoder CryptoMachineField
cryptoMachineFieldDecoder =
    object3 CryptoMachineField
        ("crypto" := cryptoDecoder)
        ("machine" := machineDecoder)
        ("value" := fieldValueDecoder)


fieldDecoder : Decoder Field
fieldDecoder =
    object5 Field
        ("code" := string)
        ("global" := nullOr fieldValueDecoder)
        ("globalCrypto" := list machineFieldDecoder)
        ("globalMachine" := list cryptoFieldDecoder)
        ("specific" := list cryptoMachineFieldDecoder)


string2machine : String -> Machine
string2machine s =
    if s == "global" then
        GlobalMachine
    else
        MachineId s


machineDecoder : Decoder Machine
machineDecoder =
    map string2machine string


stringToCrypto : String -> Crypto
stringToCrypto s =
    if s == "global" then
        GlobalCrypto
    else
        CryptoCode s


cryptoDecoder : Decoder Crypto
cryptoDecoder =
    map stringToCrypto string


displayRecDecoder : Decoder DisplayRec
displayRecDecoder =
    object2 DisplayRec
        ("code" := string)
        ("display" := string)


string2ConfigScope : String -> Result String ConfigScope
string2ConfigScope s =
    case s of
        "global" ->
            Ok Global

        "specific" ->
            Ok Specific

        "both" ->
            Ok Both

        _ ->
            Err ("No such ConfigScope " ++ s)


string2FieldType : String -> Result String FieldType
string2FieldType s =
    case s of
        "string" ->
            Ok FieldStringType

        "percentage" ->
            Ok FieldPercentageType

        "integer" ->
            Ok FieldIntegerType

        _ ->
            Err ("No such FieldType " ++ s)


configScopeDecoder : Decoder ConfigScope
configScopeDecoder =
    customDecoder string string2ConfigScope


fieldTypeDecoder : Decoder FieldType
fieldTypeDecoder =
    customDecoder string string2FieldType


fieldDescriptorDecoder : Decoder FieldDescriptor
fieldDescriptorDecoder =
    object3 FieldDescriptor
        ("code" := string)
        ("display" := string)
        ("fieldType" := fieldTypeDecoder)


configSchemaDecoder : Decoder ConfigSchema
configSchemaDecoder =
    object5 ConfigSchema
        ("code" := string)
        ("display" := string)
        ("cryptoScope" := configScopeDecoder)
        ("machineScope" := configScopeDecoder)
        ("entries" := list fieldDescriptorDecoder)


configGroupDecoder : Decoder ConfigGroup
configGroupDecoder =
    object3 ConfigGroup
        ("schema" := configSchemaDecoder)
        ("values" := list fieldDecoder)
        ("data" := configDataDecoder)


accountRecDecoder : Decoder AccountRec
accountRecDecoder =
    object3 AccountRec
        ("code" := string)
        ("display" := string)
        ("class" := string)


configDataDecoder : Decoder ConfigData
configDataDecoder =
    object4 ConfigData
        ("currencies" := list displayRecDecoder)
        ("languages" := list displayRecDecoder)
        ("accounts" := list accountRecDecoder)
        ("machines" := list machineDecoder)
