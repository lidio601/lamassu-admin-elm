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


type FieldHolder
    = ParsingError String
    | ValidationError String
    | FieldOk FieldValue
    | FieldEmpty


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
    , fieldHolder : FieldHolder
    , loadedFieldHolder : FieldHolder
    , fieldValidation : List FieldValidator
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
    | FieldFiatCurrencyType
    | FieldCryptoCurrencyType
    | FieldLanguageType


type FieldValue
    = FieldStringValue String
    | FieldPercentageValue Float
    | FieldIntegerValue Int
    | FieldOnOffValue Bool
    | FieldAccountValue String
    | FieldFiatCurrencyValue String
    | FieldCryptoCurrencyValue (List String)
    | FieldLanguageValue (List String)


type FieldValidator
    = FieldMin Int
    | FieldMax Int
    | FieldRequired


type alias FieldDescriptor =
    { code : String
    , display : String
    , fieldType : FieldType
    , fieldValidation : List FieldValidator
    , fieldClass : Maybe String
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
    { cryptoCurrencies : List CryptoDisplay
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

        FieldFiatCurrencyValue v ->
            v

        FieldCryptoCurrencyValue v ->
            Debug.crash "N/A for cryptoCurrency"

        FieldLanguageValue v ->
            Debug.crash "N/A for language"


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
            configGroup.data.cryptoCurrencies

        Global ->
            [ globalCryptoDisplay ]

        Both ->
            globalCryptoDisplay :: configGroup.data.cryptoCurrencies


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


resultToFieldHolder : Result String FieldValue -> FieldHolder
resultToFieldHolder result =
    case result of
        Ok fieldValue ->
            FieldOk fieldValue

        Err s ->
            ParsingError s


stringToFieldHolder : FieldType -> String -> FieldHolder
stringToFieldHolder fieldType s =
    if (String.isEmpty s) then
        FieldEmpty
    else
        case fieldType of
            FieldStringType ->
                FieldOk (FieldStringValue s)

            FieldPercentageType ->
                String.toFloat s
                    |> Result.map FieldPercentageValue
                    |> resultToFieldHolder

            FieldIntegerType ->
                String.toInt s
                    |> Result.map FieldIntegerValue
                    |> resultToFieldHolder

            FieldOnOffType ->
                case s of
                    "on" ->
                        FieldOk (FieldOnOffValue True)

                    "off" ->
                        FieldOk (FieldOnOffValue False)

                    _ ->
                        ParsingError ("Unsupported value for OnOff: " ++ s)

            FieldAccountType ->
                FieldOk (FieldAccountValue s)

            FieldFiatCurrencyType ->
                FieldOk (FieldFiatCurrencyValue s)

            FieldCryptoCurrencyType ->
                FieldOk (FieldCryptoCurrencyValue [ s ])

            FieldLanguageType ->
                FieldOk (FieldLanguageValue [ s ])
