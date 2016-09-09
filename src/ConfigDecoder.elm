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


inputFieldTypeDecoder : String -> Decoder FieldType
inputFieldTypeDecoder s =
    (case s of
        "string" ->
            succeed FieldStringType

        "percentage" ->
            succeed FieldPercentageType

        "integer" ->
            succeed FieldIntegerType

        "onOff" ->
            succeed FieldOnOffType

        _ ->
            fail ("No such FieldType " ++ s)
    )
        |> map FieldTypeInput


selectizeFieldTypeDecoder : String -> Decoder FieldType
selectizeFieldTypeDecoder s =
    (case s of
        "account" ->
            map FieldAccountType ("accountClass" := string)

        "currency" ->
            succeed FieldCurrencyType

        "language" ->
            succeed FieldLanguageType

        _ ->
            fail ("No such FieldType " ++ s)
    )
        |> map FieldTypeSelectize


configScopeDecoder : Decoder ConfigScope
configScopeDecoder =
    customDecoder string string2ConfigScope


fieldTypeDecoder : String -> Decoder FieldType
fieldTypeDecoder fieldType =
    oneOf [ inputFieldTypeDecoder fieldType, selectizeFieldTypeDecoder fieldType ]


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
            ("fieldInstances" := list (fieldInstanceDecoder string identity componentModelNoop))
                |> map FieldStringCluster
                |> map FieldInputCluster

        "percentage" ->
            ("fieldInstances" := list (fieldInstanceDecoder float identity componentModelNoop))
                |> map FieldPercentageCluster
                |> map FieldInputCluster

        "integer" ->
            ("fieldInstances" := list (fieldInstanceDecoder int identity componentModelNoop))
                |> map FieldIntegerCluster
                |> map FieldInputCluster

        "onOff" ->
            ("fieldInstances" := list (fieldInstanceDecoder bool identity componentModelNoop))
                |> map FieldOnOffCluster
                |> map FieldInputCluster

        "account" ->
            ("accountClass" := string)
                `andThen`
                    (\accountClass ->
                        object2 FieldAccountCluster
                            (succeed accountClass)
                            ("fieldInstances" := list (fieldInstanceDecoder string (always accountClass) (initAccountSelectize configData)))
                    )
                |> map FieldSelectizeCluster

        "currency" ->
            ("fieldInstances" := list (fieldInstanceDecoder string (always ()) (initCurrencySelectize configData)))
                |> map FieldCurrencyCluster
                |> map FieldSelectizeCluster

        "language" ->
            ("fieldInstances" := list (fieldInstanceDecoder (list string) (always ()) (initLanguageSelectize configData)))
                |> map FieldLanguageCluster
                |> map FieldSelectizeCluster

        _ ->
            fail ("Unsupported " ++ clusterTypeString)


fieldClusterDecoder : ConfigData -> Decoder FieldCluster
fieldClusterDecoder configData =
    ("fieldType" := string)
        `andThen` (fieldClusterDecoderHelper configData)


fieldGroupDecoder : ConfigData -> Decoder FieldGroup
fieldGroupDecoder configData =
    object2 FieldGroup
        ("fieldCode" := string)
        ("fieldCluster" := fieldClusterDecoder configData)


configGroupDecoderHelper : ConfigData -> Decoder ConfigGroup
configGroupDecoderHelper configData =
    object3 ConfigGroup
        ("schema" := configSchemaDecoder)
        ("values" := list (fieldGroupDecoder configData))
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
