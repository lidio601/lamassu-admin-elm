module TransactionDecoder exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Extra exposing (date)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import TransactionTypes exposing (..)


txDecode : String -> Decoder Tx
txDecode txClass =
    case txClass of
        "cashIn" ->
            map CashInTx cashInTxDecoder

        "cashOut" ->
            map CashOutTx cashOutTxDecoder

        _ ->
            fail ("Unknown tx class: " ++ txClass)


txsDecoder : Decoder (List Tx)
txsDecoder =
    (field "transactions" (list txDecoder))


txDecoder : Decoder Tx
txDecoder =
    (field "tx_class" string)
        |> andThen txDecode


cashInTxDecoder : Decoder CashInTxRec
cashInTxDecoder =
    decode CashInTxRec
        |> required "id" string
        |> required "machineName" string
        |> required "toAddress" string
        |> required "cryptoAtoms" int
        |> required "cryptoCode" string
        |> required "fiat" float
        |> required "currencyCode" string
        |> required "txHash" (nullable string)
        |> required "phone" (nullable string)
        |> required "error" (nullable string)
        |> required "created" date


cashOutTxDecoder : Decoder CashOutTxRec
cashOutTxDecoder =
    decode CashOutTxRec
        |> required "id" string
        |> required "machineName" string
        |> required "toAddress" string
        |> required "cryptoAtoms" int
        |> required "cryptoCode" string
        |> required "fiat" float
        |> required "currencyCode" string
        |> required "txHash" (nullable string)
        |> required "status" string
        |> required "dispensed" bool
        |> required "notified" bool
        |> required "redeemed" bool
        |> required "phone" (nullable string)
        |> required "error" (nullable string)
        |> required "created" date
        |> required "confirmed" bool
