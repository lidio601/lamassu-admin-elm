module ConfigDecoder exposing (..)

import Json.Decode exposing (..)
import ConfigTypes exposing (..)
import SelectizeHelpers exposing (..)


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


stringTuple : Decoder ( String, String )
stringTuple =
    tuple2 (,) string string



-- type alias FieldInstance valueType componentModel =
--     { fieldScope : FieldScope
--     , fieldValue : FieldHolder valueType
--     , loadedValue : Maybe valueType
--     , componentModel : componentModel
--     }


fieldInstanceDecoderHelper :
    (valueType -> comModParam)
    -> (comModParam -> FieldScope -> Maybe valueType -> componentModel)
    -> ( FieldScope, valueType )
    -> Decoder (FieldInstance valueType componentModel)
fieldInstanceDecoderHelper comModParamMapper comModMapper ( fieldScope, fieldValue ) =
    succeed
        { fieldScope = fieldScope
        , fieldValue = Ok (Just fieldValue)
        , loadedValue = Just fieldValue
        , componentModel = comModMapper (comModParamMapper fieldValue) fieldScope (Just fieldValue)
        }


fieldInstanceDecoder :
    Decoder valueType
    -> (valueType -> comModParam)
    -> (comModParam -> FieldScope -> Maybe valueType -> componentModel)
    -> Decoder (FieldInstance valueType componentModel)
fieldInstanceDecoder typeDecoder comModParamMapper comModMapper =
    ((object2 (,)
        ("fieldScope" := fieldScopeDecoder)
        ("fieldValue" := typeDecoder)
     )
    )
        `andThen` fieldInstanceDecoderHelper comModParamMapper comModMapper


componentModelNoop : comModParam -> FieldScope -> Maybe valueType -> ()
componentModelNoop _ _ _ =
    ()


fieldClusterDecoderHelper : ConfigData -> String -> Decoder FieldCluster
fieldClusterDecoderHelper configData clusterTypeString =
    case clusterTypeString of
        "string" ->
            object2 FieldStringCluster
                ("fieldCode" := string)
                ("fieldInstances" := list (fieldInstanceDecoder string identity componentModelNoop))

        "percentage" ->
            object2 FieldPercentageCluster
                ("fieldCode" := string)
                ("fieldInstances" := list (fieldInstanceDecoder float identity componentModelNoop))

        "integer" ->
            object2 FieldIntegerCluster
                ("fieldCode" := string)
                ("fieldInstances" := list (fieldInstanceDecoder int identity componentModelNoop))

        "onOff" ->
            object2 FieldOnOffCluster
                ("fieldCode" := string)
                ("fieldInstances" := list (fieldInstanceDecoder bool identity componentModelNoop))

        "account" ->
            object2 FieldAccountCluster
                ("fieldCode" := string)
                ("fieldInstances" := list (fieldInstanceDecoder stringTuple fst (initAccountSelectize configData)))

        "currency" ->
            object2 FieldCurrencyCluster
                ("fieldCode" := string)
                ("fieldInstances" := list (fieldInstanceDecoder string (always ()) (initCurrencySelectize configData)))

        "language" ->
            object2 FieldLanguageCluster
                ("fieldCode" := string)
                ("fieldInstances" := list (fieldInstanceDecoder (list string) (always ()) (initLanguageSelectize configData)))

        _ ->
            fail ("Unsupported " ++ clusterTypeString)



-- | FieldPercentageType
-- | FieldIntegerType
-- | FieldOnOffType
-- | FieldAccountType String
-- | FieldCurrencyType
-- | FieldLanguageType


fieldClusterDecoder : ConfigData -> Decoder FieldCluster
fieldClusterDecoder configData =
    ("fieldType" := string)
        `andThen` (fieldClusterDecoderHelper configData)


configGroupDecoderHelper : ConfigData -> Decoder ConfigGroup
configGroupDecoderHelper configData =
    object3 ConfigGroup
        ("schema" := configSchemaDecoder)
        ("values" := list (fieldClusterDecoder configData))
        (succeed configData)


configGroupDecoder : Decoder ConfigGroup
configGroupDecoder =
    ("data" := configDataDecoder)
        `andThen` configGroupDecoderHelper


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
