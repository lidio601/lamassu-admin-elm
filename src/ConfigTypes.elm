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


type alias FieldLocator =
    { fieldScope : FieldScope
    , code : String
    , fieldType : FieldType
    , fieldClass : Maybe String
    }


type FieldComponent
    = InputBoxComponent
    | SelectizeComponent Selectize.State


type alias FieldInstance =
    { fieldLocator : FieldLocator
    , component : FieldComponent
    , fieldValue : FieldHolder
    , loadedFieldValue : Maybe FieldValue
    }


type alias ResolvedFieldInstance =
    { fieldLocator : FieldLocator
    , fieldValue : Maybe FieldValue
    }


type alias Field =
    { fieldLocator : FieldLocator
    , fieldValue : FieldValue
    }


type FieldType
    = FieldStringType
    | FieldPercentageType
    | FieldIntegerType
    | FieldOnOffType
    | FieldAccountType
    | FieldCurrencyType


type FieldValue
    = FieldStringValue String
    | FieldPercentageValue Float
    | FieldIntegerValue Int
    | FieldOnOffValue Bool
    | FieldAccountValue String
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
    , cryptos : Maybe (List Crypto)
    }


accountRecToDisplayRec : AccountRec -> DisplayRec
accountRecToDisplayRec accountRec =
    { code = accountRec.code
    , display = accountRec.display
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

        FieldAccountValue v ->
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

            FieldAccountType ->
                Ok (Just (FieldAccountValue s))

            FieldCurrencyType ->
                Ok (Just (FieldCurrencyValue s))
