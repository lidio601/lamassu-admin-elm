module ConfigTypes exposing (..)

import FieldSetTypes exposing (..)


type Crypto
    = CryptoCode String
    | GlobalCrypto


type Machine
    = MachineId String
    | GlobalMachine


type alias CryptoConfig =
    { crypto : Crypto
    , machineConfigs : List MachineConfig
    }


type alias MachineConfig =
    { machine : Machine
    , fieldSet : FieldSet
    }


type alias ConfigGroup =
    { code : String
    , display : String
    , cryptoConfigs : List CryptoConfig
    }
