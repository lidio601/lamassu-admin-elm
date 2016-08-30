module ConfigTypes exposing (..)

import String


type alias DisplayRec =
    { code : String
    , display : String
    }


type Crypto
    = CryptoCode String
    | GlobalCrypto


type Machine
    = MachineId String
    | GlobalMachine


type ConfigScope
    = Global
    | Specific
    | Both


type alias Field =
    { code : String
    , crypto : Crypto
    , machine : Machine
    , fieldValue : FieldValue
    , loadedFieldValue : Maybe FieldValue
    }


type FieldType
    = FieldStringType
    | FieldPercentageType
    | FieldIntegerType


type FieldValue
    = FieldStringValue String
    | FieldPercentageValue Float
    | FieldIntegerValue Int


type alias FieldDescriptor =
    { code : String
    , display : String
    , fieldType : FieldType
    }


type alias ConfigSchema =
    { code : String
    , display : String
    , cryptoScope : ConfigScope
    , machineScope : ConfigScope
    , entries : List FieldDescriptor
    }


type alias ConfigGroup =
    { schema : ConfigSchema
    , values : List Field
    , data : ConfigData
    }


type alias AccountRec =
    { code : String
    , display : String
    , class : String
    }


type alias ConfigData =
    { cryptos : List DisplayRec
    , currencies : List DisplayRec
    , languages : List DisplayRec
    , accounts : List AccountRec
    , machines : List Machine
    }


fieldToString : Field -> String
fieldToString field =
    case field.fieldValue of
        FieldStringValue v ->
            v

        FieldPercentageValue v ->
            toString v

        FieldIntegerValue v ->
            toString v


stringToFieldValue : FieldType -> String -> Result String FieldValue
stringToFieldValue fieldType s =
    case fieldType of
        FieldStringType ->
            Ok (FieldStringValue s)

        FieldPercentageType ->
            String.toFloat s
                |> Result.map FieldPercentageValue

        FieldIntegerType ->
            String.toInt s
                |> Result.map FieldIntegerValue
