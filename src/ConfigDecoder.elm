module ConfigDecoder exposing (..)

import Json.Decode exposing (..)
import ConfigTypes exposing (..)


fieldScopeDecoder : Decoder FieldScope
fieldScopeDecoder =
    object2 FieldScope
        ("crypto" := cryptoDecoder)
        ("machine" := machineDecoder)


fieldLocatorDecoder : Decoder FieldLocator
fieldLocatorDecoder =
    object2 FieldLocator
        ("fieldScope" := fieldScopeDecoder)
        ("code" := string)


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

        "languages" ->
            succeed FieldLanguageType

        _ ->
            fail ("No such FieldType " ++ s)


configScopeDecoder : Decoder ConfigScope
configScopeDecoder =
    customDecoder string string2ConfigScope


fieldTypeDecoder : String -> Decoder FieldType
fieldTypeDecoder fieldType =
    case fieldType of
        "account" ->
            map FieldAccountType ("accountClass" := string)

        _ ->
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


fieldInstanceDecoder : Decoder valueType -> Decoder (FieldInstance valueType)
fieldInstanceDecoder typeDecoder =
    object3 FieldInstance
        ("fieldScope" := fieldScopeDecoder)
        (map (Ok << Just) ("fieldValue" := typeDecoder))
        (map Just ("fieldValue" := typeDecoder))


toFieldValue : FieldScope -> valueType -> FieldInstance valueType
toFieldValue fieldScope fieldValue =
    { fieldScope = fieldScope
    , fieldValue = Ok (Just fieldValue)
    , loadedValue = Just fieldValue
    }


fieldInstancesDecoder : Decoder valueType -> Decoder (FieldInstance valueType)
fieldInstancesDecoder typeDecoder =
    (object2 toFieldValue
        ("fieldScope" := fieldScopeDecoder)
        ("fieldValue" := typeDecoder)
    )


fieldClusterDecoderHelper : String -> Decoder FieldCluster
fieldClusterDecoderHelper clusterTypeString =
    case clusterTypeString of
        "string" ->
            object2 FieldStringCluster
                ("fieldCode" := string)
                ("fieldInstances" := list (fieldInstanceDecoder string))



-- | FieldPercentageType
-- | FieldIntegerType
-- | FieldOnOffType
-- | FieldAccountType String
-- | FieldCurrencyType
-- | FieldLanguageType


fieldClusterDecoder : Decoder FieldCluster
fieldClusterDecoder =
    ("fieldType" := string) `andThen` fieldClusterDecoderHelper


configGroupDecoder : Decoder ConfigGroup
configGroupDecoder =
    object3 ConfigGroup
        ("schema" := configSchemaDecoder)
        ("values" := list fieldClusterDecoder)
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
