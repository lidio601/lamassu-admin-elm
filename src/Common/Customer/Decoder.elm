module Common.Customer.Decoder exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Extra exposing (date, fromResult)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Common.Customer.Types exposing (..)


customersDecoder : Decoder (List Customer)
customersDecoder =
    field "customers" (list customerDecoder)


mapAuthorizedTypes : String -> Decoder Authorized
mapAuthorizedTypes s =
    case s of
        "blocked" ->
            succeed Blocked

        "verified" ->
            succeed Verified

        "automatic" ->
            succeed Automatic

        _ ->
            fail ("No such type " ++ s)


authorizedDecoder : Decoder Authorized
authorizedDecoder =
    string
        |> andThen mapAuthorizedTypes


customerDecoder : Decoder Customer
customerDecoder =
    decode Customer
        |> required "id" string
        |> required "name" (nullable string)
        |> required "phone" (nullable string)
        |> required "phoneAt" (nullable date)
        |> required "smsOverride" authorizedDecoder
        |> required "created" date
        |> required "status" (nullable string)
        |> required "authorizedOverride" authorizedDecoder
        |> required "authorizedAt" (nullable date)
        |> required "idCardData" (nullable string)
        |> required "idCardDataOverride" authorizedDecoder
        |> required "idCardAt" (nullable date)
        |> required "idCardImagePath" (nullable string)
        |> required "idCardPhotoOverride" authorizedDecoder
        |> required "idCardImageAt" (nullable date)
        |> required "sanctionsCheck" (nullable string)
        |> required "sanctionsCheckOverride" authorizedDecoder
        |> required "sanctionsCheckAt" (nullable date)
        |> required "frontFacingCamPath" (nullable string)
        |> required "frontFacingCamOverride" authorizedDecoder
        |> required "frontFacingCamAt" (nullable date)
