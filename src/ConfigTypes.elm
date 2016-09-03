module ConfigTypes exposing (..)

import String
import Selectize


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


type alias MachineDisplay =
    { machine : Machine
    , display : String
    }


type alias CryptoDisplay =
    { crypto : Crypto
    , display : String
    }


type ConfigScope
    = Global
    | Specific
    | Both


type FieldError
    = FieldParsingError String
    | FieldValidationError String


type alias FieldHolder =
    Result FieldError (Maybe FieldValue)


type alias FieldScope =
    { crypto : Crypto
    , machine : Machine
    }


type FieldComponent
    = FieldStringComponent
    | FieldPercentageComponent
    | FieldIntegerComponent
    | FieldOnOffComponent
    | FieldAccountComponent String Selectize.Model
    | FieldCurrencyComponent Selectize.Model


type alias FieldInstance =
    { crypto : Crypto
    , machine : Machine
    , code : String
    , component : FieldComponent
    }


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
    | FieldOnOffType
    | FieldAccountType String
    | FieldCurrencyType


type FieldValue
    = FieldStringValue String
    | FieldPercentageValue Float
    | FieldIntegerValue Int
    | FieldOnOffValue Bool
    | FieldAccountValue String String
    | FieldCurrencyValue String


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
    { cryptos : List CryptoDisplay
    , currencies : List DisplayRec
    , languages : List DisplayRec
    , accounts : List AccountRec
    , machines : List MachineDisplay
    }


globalCryptoDisplay : CryptoDisplay
globalCryptoDisplay =
    { crypto = GlobalCrypto
    , display = "Global"
    }


globalMachineDisplay : MachineDisplay
globalMachineDisplay =
    { machine = GlobalMachine
    , display = "Global"
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

        FieldOnOffValue v ->
            if v then
                "on"
            else
                "off"

        FieldAccountValue _ v ->
            v

        FieldCurrencyValue v ->
            v


machineToString : Machine -> String
machineToString machine =
    case machine of
        GlobalMachine ->
            "global"

        MachineId machineId ->
            machineId


cryptoToString : Crypto -> String
cryptoToString crypto =
    case crypto of
        GlobalCrypto ->
            "global"

        CryptoCode code ->
            code


listMachines : ConfigGroup -> List MachineDisplay
listMachines configGroup =
    case configGroup.schema.machineScope of
        Specific ->
            configGroup.data.machines

        Global ->
            [ globalMachineDisplay ]

        Both ->
            globalMachineDisplay :: configGroup.data.machines


listCryptos : ConfigGroup -> List CryptoDisplay
listCryptos configGroup =
    case configGroup.schema.cryptoScope of
        Specific ->
            configGroup.data.cryptos

        Global ->
            [ globalCryptoDisplay ]

        Both ->
            globalCryptoDisplay :: configGroup.data.cryptos


fieldScopes : ConfigGroup -> List FieldScope
fieldScopes configGroup =
    let
        machines =
            List.map .machine (listMachines configGroup)

        cryptos =
            List.map .crypto (listCryptos configGroup)

        cryptoScopes crypto =
            List.map (\machine -> { machine = machine, crypto = crypto }) machines
    in
        List.concatMap cryptoScopes cryptos


stringToCrypto : String -> Crypto
stringToCrypto string =
    case string of
        "global" ->
            GlobalCrypto

        _ ->
            CryptoCode string


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

            FieldOnOffType ->
                case s of
                    "on" ->
                        Ok (Just (FieldOnOffValue True))

                    "off" ->
                        Ok (Just (FieldOnOffValue False))

                    _ ->
                        Err (FieldParsingError ("Unsupported value for OnOff: " ++ s))

            FieldAccountType accountCode ->
                Ok (Just (FieldAccountValue accountCode s))

            FieldCurrencyType ->
                Ok (Just (FieldCurrencyValue s))
