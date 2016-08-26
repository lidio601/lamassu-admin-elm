module ConfigTypes exposing (..)

import FieldSetTypes exposing (FieldStatus)
import String


type alias Field =
    { code : String
    , display : String
    , required : Bool
    , value : FieldValue
    , loadedValue : FieldValue
    , status : FieldStatus
    }


type FieldValue
    = FieldString String
    | FieldPercentage Int


type alias FieldSet =
    { fields : List Field
    }


type Crypto
    = CryptoCode String
    | GlobalCrypto


type Machine
    = MachineId String
    | GlobalMachine


type alias MachineConfig =
    { machine : Machine
    , fieldSet : FieldSet
    }


type alias CryptoConfig =
    { crypto : Crypto
    , machineConfigs : List MachineConfig
    }


type alias CryptoDescriptor =
    { crypto : Crypto
    , display : String
    }


type alias ConfigGroup =
    { code : String
    , display : String
    , cryptoConfigs : List CryptoConfig
    , cryptos : List CryptoDescriptor
    }


updateFieldValue : String -> FieldValue -> FieldValue
updateFieldValue stringValue oldFieldValue =
    case oldFieldValue of
        FieldString _ ->
            FieldString stringValue

        FieldPercentage oldPct ->
            String.toInt stringValue
                |> Result.withDefault 0
                |> FieldPercentage
