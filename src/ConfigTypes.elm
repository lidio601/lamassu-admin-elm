module ConfigTypes exposing (..)

import FieldSetTypes exposing (FieldStatus)
import String


type alias DisplayRec =
    { code : String
    , display : String
    }


type alias Field =
    { code : String
    , display : String
    , value : FieldValue
    , loadedValue : FieldValue
    , status : FieldStatus
    }


type FieldValue
    = FieldString (Maybe String)
    | FieldPercentage (Maybe Float)
    | FieldInteger (Maybe Int)


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


type ConfigScope
    = Global
    | Specific
    | Both


type alias ConfigGroup =
    { group : DisplayRec
    , cryptoScope : ConfigScope
    , machineScope : ConfigScope
    , cryptoConfigs : List CryptoConfig
    , data : ConfigData
    }


type alias AccountRec =
    { code : String
    , display : String
    , class : String
    }


type alias ConfigData =
    { currencies : List DisplayRec
    , languages : List DisplayRec
    , accounts : List AccountRec
    }


updateFieldValue : String -> FieldValue -> FieldValue
updateFieldValue stringValue oldFieldValue =
    case oldFieldValue of
        FieldString _ ->
            if (String.isEmpty stringValue) then
                FieldString Nothing
            else
                FieldString (Just stringValue)

        FieldPercentage oldPct ->
            String.toFloat stringValue
                |> Result.toMaybe
                |> FieldPercentage

        FieldInteger oldInt ->
            String.toInt stringValue
                |> Result.toMaybe
                |> FieldInteger
