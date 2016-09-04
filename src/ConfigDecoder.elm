module ConfigDecoder exposing (..)

import Json.Decode exposing (..)
import ConfigTypes exposing (..)


fieldValueDecoder : String -> Decoder FieldValue
fieldValueDecoder fieldType =
    case fieldType of
        "string" ->
            map FieldStringValue ("fieldValue" := string)

        "percentage" ->
            map FieldPercentageValue ("fieldValue" := float)

        "integer" ->
            map FieldIntegerValue ("fieldValue" := int)

        "onOff" ->
            map FieldOnOffValue ("fieldValue" := bool)

        "currency" ->
            map FieldCurrencyValue ("fieldValue" := string)

        _ ->
            fail ("Unsupported field type: " ++ fieldType)


fieldScopeDecoder : Decoder FieldScope
fieldScopeDecoder =
    object2 FieldScope
        ("crypto" := cryptoDecoder)
        ("machine" := machineDecoder)


fieldLocatorDecoder : Decoder FieldLocator
fieldLocatorDecoder =
    object2 FieldLocator
        ("scope" := fieldScopeDecoder)
        ("code" := string)


fieldDecoder : Decoder Field
fieldDecoder =
    object3 Field
        ("locator" := fieldLocatorDecoder)
        (map (Ok << Just)
            (("fieldType" := string)
                `andThen` fieldValueDecoder
            )
        )
        (map Just
            (("fieldType" := string)
                `andThen` fieldValueDecoder
            )
        )


string2machine : String -> Machine
string2machine s =
    if s == "global" then
        GlobalMachine
    else
        MachineId s


machineDecoder : Decoder Machine
machineDecoder =
    map string2machine string


cryptoDecoder : Decoder Crypto
cryptoDecoder =
    map stringToCrypto string


displayRecDecoder : Decoder DisplayRec
displayRecDecoder =
    object2 DisplayRec
        ("code" := string)
        ("display" := string)


machineDisplayDecoder : Decoder MachineDisplay
machineDisplayDecoder =
    object2 MachineDisplay
        ("machine" := machineDecoder)
        ("display" := string)


cryptoDisplayDecoder : Decoder CryptoDisplay
cryptoDisplayDecoder =
    object2 CryptoDisplay
        ("crypto" := cryptoDecoder)
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

        "onOff" ->
            Ok FieldOnOffType

        "currency" ->
            Ok FieldCurrencyType

        _ ->
            Err ("No such FieldType " ++ s)


configScopeDecoder : Decoder ConfigScope
configScopeDecoder =
    customDecoder string string2ConfigScope


fieldTypeDecoder : String -> Decoder FieldType
fieldTypeDecoder fieldType =
    case fieldType of
        "account" ->
            map FieldAccountType ("accountClass" := string)

        _ ->
            basicFieldTypeDecoder


basicFieldTypeDecoder : Decoder FieldType
basicFieldTypeDecoder =
    customDecoder string string2FieldType


fieldDescriptorDecoder : Decoder FieldDescriptor
fieldDescriptorDecoder =
    object3 FieldDescriptor
        ("code" := string)
        ("display" := string)
        (("fieldType" := string) `andThen` fieldTypeDecoder)


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
    object5 ConfigData
        ("cryptos" := list cryptoDisplayDecoder)
        ("currencies" := list displayRecDecoder)
        ("languages" := list displayRecDecoder)
        ("accounts" := list accountRecDecoder)
        ("machines" := list machineDisplayDecoder)
