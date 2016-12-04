module Transaction exposing (..)

import Html exposing (..)
import Html.Attributes exposing (colspan)
import Css.Admin exposing (..)
import Css.Classes as C
import RemoteData exposing (..)
import Http
import HttpBuilder exposing (..)
import TransactionDecoder exposing (txsDecoder)
import TransactionTypes exposing (..)
import List
import Numeral exposing (format)
import Date.Extra exposing (toFormattedString)


type alias Txs =
    List Tx


type alias Model =
    RemoteData.WebData Txs


init : Model
init =
    NotAsked


loadCmd : Cmd Msg
loadCmd =
    getTransactions


load : ( Model, Cmd Msg )
load =
    ( Loading, loadCmd )


getTransactions : Cmd Msg
getTransactions =
    get ("/api/transactions")
        |> withExpect (Http.expectJson txsDecoder)
        |> send RemoteData.fromResult
        |> Cmd.map Load


type Msg
    = Load Model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load loadedModel ->
            loadedModel ! []


rowView : Tx -> Html Msg
rowView tx =
    case tx of
        CashInTx cashIn ->
            tr []
                [ td [ class [ C.NumberColumn, C.DateColumn ] ] [ text (toFormattedString "yyyy-MM-dd HH:mm" cashIn.created) ]
                , td [] [ text cashIn.machineName ]
                , td [ class [ C.NumberColumn ] ] [ text (format "0,0.000000" ((negate <| toFloat cashIn.cryptoAtoms) / 1.0e8)) ]
                , td [] [ text cashIn.cryptoCode ]
                , td [ class [ C.NumberColumn ] ] [ text (format "0,0.00" cashIn.fiat) ]
                , td [ class [ C.NumberColumn ] ] [ text (Maybe.withDefault "" cashIn.phone) ]
                , td [ class [ C.NumberColumn, C.TruncatedColumn ] ] [ text cashIn.toAddress ]
                ]

        CashOutTx cashOut ->
            tr []
                [ td [ class [ C.NumberColumn, C.DateColumn ] ] [ text (toFormattedString "yyyy-MM-dd HH:mm" cashOut.created) ]
                , td [] [ text cashOut.machineName ]
                , td [ class [ C.NumberColumn ] ] [ text (format "0,0.000000" ((toFloat cashOut.cryptoAtoms) / 1.0e8)) ]
                , td [] [ text cashOut.cryptoCode ]
                , td [ class [ C.NumberColumn ] ] [ text (format "0,0.00" (negate cashOut.fiat)) ]
                , td [ class [ C.NumberColumn ] ] [ text (Maybe.withDefault "" cashOut.phone) ]
                , td [ class [ C.NumberColumn, C.TruncatedColumn ] ] [ text cashOut.toAddress ]
                ]


tableView : Txs -> Html Msg
tableView txs =
    if List.isEmpty txs then
        div [] [ text "No activity yet." ]
    else
        table [ class [ C.TxTable ] ]
            [ thead []
                [ tr []
                    [ td [ class [ C.DateColumn ] ] []
                    , td [] []
                    , td [ colspan 2 ] [ text "Crypto" ]
                    , td [] [ text "Fiat" ]
                    , td [] [ text "Phone" ]
                    , td [] [ text "To address" ]
                    ]
                ]
            , tbody [] (List.map rowView txs)
            ]


view : Model -> Html Msg
view model =
    case model of
        NotAsked ->
            div [] []

        Loading ->
            div [] [ text "Loading..." ]

        Failure err ->
            div [] [ text (toString err) ]

        Success txs ->
            div [] [ tableView txs ]
