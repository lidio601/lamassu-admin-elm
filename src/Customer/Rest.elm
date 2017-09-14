module Customer.Rest exposing (..)

import RemoteData exposing (..)
import Http
import HttpBuilder exposing (..)
import Common.Customer.Decoder exposing (customerDecoder)
import Customer.Types exposing (..)


updateCustomer : String -> String -> Cmd Msg
updateCustomer id action =
    patch ("/api/customer/" ++ id ++ "?authorizedOverride=" ++ action)
        |> withExpect (Http.expectJson customerDecoder)
        |> send RemoteData.fromResult
        |> Cmd.map Load


getCustomer : String -> Cmd Msg
getCustomer id =
    get ("/api/customer/" ++ id)
        |> withExpect (Http.expectJson customerDecoder)
        |> send RemoteData.fromResult
        |> Cmd.map Load
