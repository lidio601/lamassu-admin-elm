module ConfigDecoder exposing (..)

import Json.Decode exposing (..)
import ConfigTypes exposing (..)


fieldScopeDecoder : Decoder FieldScope
fieldScopeDecoder =
    object2 FieldScope
        ("crypto" := cryptoDecoder)
        ("machine" := machineDecoder)


fieldCodeDecoder : Decoder FieldCode
fieldCodeDecoder =
    oneOf
        [ (object2 FieldCode
            ("fieldName" := string)
            (("fieldClass" := string) |> map Just)
          )
        , (object2 FieldCode
            ("fieldName" := string)
            (succeed Nothing)
          )
        ]


fieldLocatorDecoder : Decoder FieldLocator
fieldLocatorDecoder =
    object2 FieldLocator
        ("fieldScope" := fieldScopeDecoder)
        ("fieldCode" := fieldCodeDecoder)


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
            succeed FieldAccountType

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
        ("fieldCode" := fieldCodeDecoder)
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


fieldDecoder : Decoder Field
fieldDecoder =
    object2 Field
        ("fieldLocator" := fieldLocatorDecoder)
        ("fieldValue" := value)


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


configGroupDecoderHelper : ConfigData -> Decoder ConfigGroup
configGroupDecoderHelper configData =
    object3 ConfigGroup
        ("schema" := configSchemaDecoder)
        ("values" := list fieldDecoder)
        (succeed configData)


configGroupDecoder : Decoder ConfigGroup
configGroupDecoder =
    ("data" := configDataDecoder)
        `andThen` configGroupDecoderHelper


configDataDecoder : Decoder ConfigData
configDataDecoder =
    object5 ConfigData
        ("cryptos" := list cryptoDisplayDecoder)
        ("currencies" := list displayRecDecoder)
        ("languages" := list displayRecDecoder)
        ("accounts" := list accountRecDecoder)
        ("machines" := list machineDisplayDecoder)
