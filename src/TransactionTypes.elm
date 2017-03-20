module TransactionTypes exposing (..)

import Date exposing (Date)


type alias CashInTxRec =
    { id : String
    , machineName : String
    , toAddress : String
    , cryptoAtoms : Int
    , cryptoCode : String
    , fiat : Float
    , fiatCode : String
    , txHash : Maybe String
    , phone : Maybe String
    , error : Maybe String
    , created : Date
    }


type alias CashOutTxRec =
    { id : String
    , machineName : String
    , toAddress : String
    , cryptoAtoms : Int
    , cryptoCode : String
    , fiat : Float
    , fiatCode : String
    , txHash : Maybe String
    , status : String
    , dispensed : Bool
    , notified : Bool
    , redeemed : Bool
    , phone : Maybe String
    , error : Maybe String
    , created : Date
    , confirmed : Bool
    }


type Tx
    = CashInTx CashInTxRec
    | CashOutTx CashOutTxRec
