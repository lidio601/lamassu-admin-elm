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
    Json.Encode.object
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


encodeFieldLocator : FieldLocator -> Value
encodeFieldLocator fieldLocator =
    Json.Encode.object
        [ ( "fieldScope", encodeFieldScope fieldLocator.fieldScope )
        , ( "code", string fieldLocator.code )
        , ( "fieldType", fieldTypeEncoder fieldLocator.fieldType )
        , ( "fieldClass", maybeString fieldLocator.fieldClass )
        ]


encodeFieldValue : FieldValue -> Value
encodeFieldValue fieldValue =
    case fieldValue of

encodeField : Field -> Value
encodeField field =
    object
        [ ( "fieldLocator", encodeFieldLocator field.fieldLocator )
        , ( "fieldValue", encodeFieldValue field.fieldValue )
        ]


encodeResults : String -> List Field -> Value
encodeResults configGroupCode fields =
    Json.Encode.object
        [ ( "groupCode", string configGroupCode )
        , ( "values", list (List.filterMap encodeField fields) )
        ]
