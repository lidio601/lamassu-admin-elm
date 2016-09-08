module ConfigTypes exposing (..)

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


type alias FieldHolder valueType =
    Result FieldError (Maybe valueType)


type alias FieldScope =
    { crypto : Crypto
    , machine : Machine
    }


type alias FieldLocator =
    { fieldScope : FieldScope
    , code : String
    }


type FieldCluster
    = FieldStringCluster (FieldInstance String)
    | FieldPercentageCluster (FieldInstance Float)
    | FieldIntegerCluster (FieldInstance Int)
    | FieldOnOffCluster (FieldInstance Bool)
    | FieldAccountCluster (FieldInstance ( String, String )) (Selectize.Model String)
    | FieldCurrencyCluster (FieldInstance String) (Selectize.Model String)
    | FieldLanguageCluster (FieldInstance (List String)) (Selectize.Model String)


type alias FieldInstance valueType =
    { fieldScope : FieldScope
    , value : FieldHolder valueType
    , loadedValue : Maybe valueType
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
    | FieldAccountType String
    | FieldCurrencyType
    | FieldLanguageType


type FieldValue
    = FieldStringValue String
    | FieldPercentageValue Float
    | FieldIntegerValue Int
    | FieldOnOffValue Bool
    | FieldAccountValue String String
    | FieldCurrencyValue String
    | FieldLanguageValue (List String)


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

        FieldLanguageValue v ->
            Debug.crash "Can't turn languages into string"


fieldValueToList : FieldValue -> List String
fieldValueToList fieldValue =
    case fieldValue of
        FieldStringValue v ->
            [ v ]

        FieldPercentageValue v ->
            [ toString v ]

        FieldIntegerValue v ->
            [ toString v ]

        FieldOnOffValue v ->
            if v then
                [ "on" ]
            else
                [ "off" ]

        FieldAccountValue _ v ->
            [ v ]

        FieldCurrencyValue v ->
            [ v ]

        FieldLanguageValue v ->
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
