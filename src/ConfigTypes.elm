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
    , fieldType : FieldType
    , fieldClass : Maybe String
    }


type alias FieldInstance valueType componentType =
    { fieldScope : FieldScope
    , fieldHolder : FieldHolder valueType
    , loadedValue : Maybe valueType
    , component : componentType
    }


type SelectizeCluster
    = AccountCluster (List (FieldInstance String Selectize.State))
    | CurrencyCluster (List (FieldInstance String Selectize.State))
    | LanguageCluster (List (FieldInstance (List String) Selectize.State))


type InputCluster
    = StringCluster (List (FieldInstance String ()))
    | PercentageCluster (List (FieldInstance Float ()))
    | IntegerCluster (List (FieldInstance Int ()))
    | OnOffCluster (List (FieldInstance Bool ()))


type alias Suite clusterType =
    { code : String
    , cluster : clusterType
    }


type alias FieldGroup =
    { selectize : List (Suite SelectizeCluster)
    , input : List (Suite InputCluster)
    }


type alias Field =
    { fieldLocator : FieldLocator
    , fieldValue : Maybe FieldValue
    }


type FieldType
    = StringType
    | PercentageType
    | IntegerType
    | OnOffType
    | AccountType
    | CurrencyType
    | LanguageType


type FieldValue
    = StringValue String
    | PercentageValue Float
    | IntegerValue Int
    | OnOffValue Bool
    | AccountValue String
    | CurrencyValue String
    | LanguageValue (List String)


type alias FieldDescriptor =
    { code : String
    , display : String
    , fieldType : FieldType
    , fieldClass : Maybe String
    }


type alias ConfigSchema =
    { code : String
    , display : String
    , cryptoScope : ConfigScope
    , machineScope : ConfigScope
    , fieldDescriptors : List FieldDescriptor
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
