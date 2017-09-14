module Common.Customer.Decoder exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Extra exposing (date, fromResult)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Common.Customer.Types exposing (Customer)


customersDecoder : Decoder (List Customer)
customersDecoder =
    field "customers" (list customerDecoder)


customerDecoder : Decoder Customer
customerDecoder =
    decode Customer
        |> required "id" string
        |> required "name" (nullable string)
        |> required "phone" (nullable string)
        |> required "phoneAt" (nullable date)
        |> required "created" date
        |> required "status" (nullable string)
        |> required "authorizedOverride" (nullable string)
