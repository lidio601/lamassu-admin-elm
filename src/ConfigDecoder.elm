module ConfigDecoder exposing (..)

import Json.Decode exposing (..)
import ConfigTypes exposing (..)


fieldValueTypeDecoder : String -> Decoder FieldValue
fieldValueTypeDecoder fieldType =
    case fieldType of
        "string" ->
            map FieldStringValue ("value" := string)

        "percentage" ->
            map FieldPercentageValue ("value" := float)

        "integer" ->
            map FieldIntegerValue ("value" := int)

        "onOff" ->
            map FieldOnOffValue ("value" := bool)

        "currency" ->
            map FieldCurrencyValue ("value" := string)

        "account" ->
            map FieldCurrencyValue ("value" := string)

        _ ->
            fail ("Unsupported field type: " ++ fieldType)


fieldValueDecoder : Decoder FieldValue
fieldValueDecoder =
    ("fieldType" := string) `andThen` fieldValueTypeDecoder


fieldScopeDecoder : Decoder FieldScope
fieldScopeDecoder =
    object2 FieldScope
        ("crypto" := cryptoDecoder)
        ("machine" := machineDecoder)


nullOr : Decoder a -> Decoder (Maybe a)
nullOr decoder =
    oneOf
        [ null Nothing
        , map Just decoder
        ]


fieldLocatorDecoder : Decoder FieldLocator
fieldLocatorDecoder =
    object4 FieldLocator
        ("fieldScope" := fieldScopeDecoder)
        ("code" := string)
        (("fieldType" := string) `andThen` fieldTypeDecoder)
        ("fieldClass" := nullOr string)


fieldDecoder : Decoder Field
fieldDecoder =
    object2 Field
        ("fieldLocator" := fieldLocatorDecoder)
        ("fieldValue" := fieldValueDecoder)


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


basicFieldTypeDecoder : String -> Decoder FieldType
basicFieldTypeDecoder s =
    case s of
        "string" ->
            succeed FieldStringType

        "percentage" ->
            succeed FieldPercentageType

        "integer" ->
            succeed FieldIntegerType

        "onOff" ->
            succeed FieldOnOffType

        "currency" ->
            succeed FieldCurrencyType

        _ ->
            fail ("No such FieldType " ++ s)


configScopeDecoder : Decoder ConfigScope
configScopeDecoder =
    customDecoder string string2ConfigScope


fieldTypeDecoder : String -> Decoder FieldType
fieldTypeDecoder fieldType =
    basicFieldTypeDecoder fieldType


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
    oneOf
        [ object4 AccountRec
            ("code" := string)
            ("display" := string)
            ("class" := string)
            ("cryptos" := map Just (list cryptoDecoder))
        , object4 AccountRec
            ("code" := string)
            ("display" := string)
            ("class" := string)
            (succeed Nothing)
        ]


configDataDecoder : Decoder ConfigData
configDataDecoder =
    object5 ConfigData
        ("cryptos" := list cryptoDisplayDecoder)
        ("currencies" := list displayRecDecoder)
        ("languages" := list displayRecDecoder)
        ("accounts" := list accountRecDecoder)
        ("machines" := list machineDisplayDecoder)
