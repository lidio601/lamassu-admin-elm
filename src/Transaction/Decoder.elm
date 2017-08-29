module Transaction.Decoder exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Extra exposing (date, fromResult)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Common.TransactionTypes exposing (..)
import String


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
    (field "txClass" string)
        |> andThen txDecode


floatString : Decoder Float
floatString =
    string |> andThen (String.toFloat >> fromResult)


intString : Decoder Int
intString =
    string |> andThen (String.toInt >> fromResult)


cashInTxDecoder : Decoder CashInTxRec
cashInTxDecoder =
    decode CashInTxRec
        |> required "id" string
        |> required "machineName" string
        |> required "toAddress" string
        |> required "cryptoAtoms" intString
        |> required "cryptoCode" string
        |> required "fiat" floatString
        |> required "fiatCode" string
        |> required "txHash" (nullable string)
        |> required "phone" (nullable string)
        |> required "error" (nullable string)
        |> required "operatorCompleted" bool
        |> required "send" bool
        |> required "sendConfirmed" bool
        |> required "expired" bool
        |> required "created" date


confirmedDecoder : Decoder Bool
confirmedDecoder =
    map (Maybe.map (always True) >> Maybe.withDefault False)
        (nullable string)


cashOutTxDecoder : Decoder CashOutTxRec
cashOutTxDecoder =
    decode CashOutTxRec
        |> required "id" string
        |> required "machineName" string
        |> required "toAddress" string
        |> required "cryptoAtoms" intString
        |> required "cryptoCode" string
        |> required "fiat" floatString
        |> required "fiatCode" string
        |> required "status" string
        |> required "dispense" bool
        |> required "notified" bool
        |> required "redeem" bool
        |> required "phone" (nullable string)
        |> required "error" (nullable string)
        |> required "created" date
        |> required "confirmationTime" confirmedDecoder
