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


type FieldError
    = FieldParsingError String
    | FieldValidationError String


type alias FieldHolder =
    Result FieldError (Maybe FieldValue)


type alias Field =
    { code : String
    , crypto : Crypto
    , machine : Machine
    , fieldValue : FieldHolder
    , loadedFieldValue : Maybe FieldValue
    }


type alias ValidDirtyField =
    { code : String
    , crypto : Crypto
    , machine : Machine
    , fieldValue : Maybe FieldValue
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


fieldValueToString : FieldValue -> String
fieldValueToString fieldValue =
    case fieldValue of
        FieldStringValue v ->
            v

        FieldPercentageValue v ->
            toString v

        FieldIntegerValue v ->
            toString v


machineToString : Machine -> String
machineToString machine =
    case machine of
        GlobalMachine ->
            "global"

        MachineId machineId ->
            machineId


stringToFieldValue : FieldType -> String -> FieldHolder
stringToFieldValue fieldType s =
    if (String.isEmpty s) then
        Ok Nothing
    else
        case fieldType of
            FieldStringType ->
                Ok (Just (FieldStringValue s))

            FieldPercentageType ->
                String.toFloat s
                    |> Result.map FieldPercentageValue
                    |> Result.map Just
                    |> Result.formatError FieldParsingError

            FieldIntegerType ->
                String.toInt s
                    |> Result.map FieldIntegerValue
                    |> Result.map Just
                    |> Result.formatError FieldParsingError
