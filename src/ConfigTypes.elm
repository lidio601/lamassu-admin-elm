module ConfigTypes exposing (..)

import FieldSetTypes exposing (..)


type Crypto
    = CryptoCode String
    | GlobalCrypto


type Machine
    = MachineId String
    | GlobalMachine


type alias MachineConfig =
    FieldSetRow Machine


type alias CryptoConfig =
    FieldSetTable Crypto Machine


type alias ConfigGroup =
    { code : String
    , display : String
    , cryptoConfigs : List CryptoConfig
    }
