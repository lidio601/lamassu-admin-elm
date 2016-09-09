module ConfigTypes exposing (..)

import Selectize
import Json.Decode


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


type alias FieldHolder valueType =
    Result FieldError (Maybe valueType)


type alias FieldScope =
    { crypto : Crypto
    , machine : Machine
    }


type alias FieldLocator =
    { fieldScope : FieldScope
    , fieldCode : FieldCode
    }


type alias SelectizeModel =
    Selectize.Model String


type alias FieldInstance valueType componentModel =
    { fieldScope : FieldScope
    , fieldValue : FieldHolder valueType
    , loadedValue : Maybe valueType
    , componentModel : componentModel
    }


type InputCluster
    = FieldStringCluster (List (FieldInstance String ()))
    | FieldPercentageCluster (List (FieldInstance Float ()))
    | FieldIntegerCluster (List (FieldInstance Int ()))
    | FieldOnOffCluster (List (FieldInstance Bool ()))


type SelectizeCluster
    = FieldAccountCluster (List (FieldInstance String SelectizeModel))
    | FieldCurrencyCluster (List (FieldInstance String SelectizeModel))
    | FieldLanguageCluster (List (FieldInstance (List String) SelectizeModel))


type FieldCluster
    = FieldInputCluster InputCluster
    | FieldSelectizeCluster SelectizeCluster


type alias FieldGroup =
    { fieldCode : FieldCode
    , fieldCluster : FieldCluster
    , fieldClass : String
    }


type alias FieldCode =
    { fieldName : String
    , fieldClass : Maybe String
    }


type SelectizeFieldType
    = FieldAccountType String
    | FieldCurrencyType
    | FieldLanguageType


type InputFieldType
    = FieldStringType
    | FieldPercentageType
    | FieldIntegerType
    | FieldOnOffType


type FieldType
    = FieldTypeInput InputFieldType
    | FieldTypeSelectize SelectizeFieldType


type alias FieldDescriptor =
    { fieldCode : FieldCode
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


type alias Field =
    { fieldLocator : FieldLocator
    , fieldValue : Json.Decode.Value
    }


type alias ConfigGroup =
    { schema : ConfigSchema
    , fieldValues : List Field
    , data : ConfigData
    }


type alias AccountRec =
    { code : String
    , display : String
    , class : String
    , cryptos : Maybe (List Crypto)
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



-- fieldValueToString : FieldValue -> String
-- fieldValueToString fieldValue =
--     case fieldValue of
--         FieldStringValue v ->
--             v
--
--         FieldPercentageValue v ->
--             toString v
--
--         FieldIntegerValue v ->
--             toString v
--
--         FieldOnOffValue v ->
--             if v then
--                 "on"
--             else
--                 "off"
--
--         FieldAccountValue _ v ->
--             v
--
--         FieldCurrencyValue v ->
--             v
--
--         FieldLanguageValue v ->
--             Debug.crash "Can't turn languages into string"


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
