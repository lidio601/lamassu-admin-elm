module ConfigEncoder exposing (..)

import Json.Encode exposing (..)
import List
import ConfigTypes exposing (..)


encodeCrypto : Crypto -> Value
encodeCrypto crypto =
    case crypto of
        CryptoCode cryptoCode ->
            string cryptoCode

        GlobalCrypto ->
            string "global"


encodeMachine : Machine -> Value
encodeMachine machine =
    case machine of
        MachineId machineId ->
            string machineId

        GlobalMachine ->
            string "global"


encodeFieldScope : FieldScope -> Value
encodeFieldScope fieldScope =
    object
        [ ( "crypto", encodeCrypto fieldScope.crypto )
        , ( "machine", encodeMachine fieldScope.machine )
        ]


maybeString : Maybe String -> Value
maybeString maybeString =
    case maybeString of
        Nothing ->
            null

        Just s ->
            string s


encodeFieldType : FieldType -> Value
encodeFieldType fieldType =
    case fieldType of
        StringType ->
            string "string"

        PercentageType ->
            string "percentage"

        IntegerType ->
            string "integer"

        OnOffType ->
            string "onOff"

        AccountType ->
            string "account"

        CurrencyType ->
            string "currency"

        LanguageType ->
            string "language"


encodeFieldLocator : FieldLocator -> Value
encodeFieldLocator fieldLocator =
    object
        [ ( "fieldScope", encodeFieldScope fieldLocator.fieldScope )
        , ( "code", string fieldLocator.code )
        , ( "fieldType", encodeFieldType fieldLocator.fieldType )
        , ( "fieldClass", maybeString fieldLocator.fieldClass )
        ]


encodeFieldValue : Maybe FieldValue -> Value
encodeFieldValue fieldValue =
    case fieldValue of
        Nothing ->
            null

        Just value ->
            case value of
                StringValue v ->
                    string v

                PercentageValue v ->
                    float v

                IntegerValue v ->
                    int v

                OnOffValue v ->
                    bool v

                AccountValue v ->
                    string v

                CurrencyValue v ->
                    string v

                LanguageValue v ->
                    list (List.map string v)


encodeField : Field -> Value
encodeField field =
    object
        [ ( "fieldLocator", encodeFieldLocator field.fieldLocator )
        , ( "fieldValue", encodeFieldValue field.fieldValue )
        ]


encodeResults : String -> List Field -> Value
encodeResults configGroupCode fields =
    object
        [ ( "groupCode", string configGroupCode )
        , ( "values", list (List.map encodeField fields) )
        ]
