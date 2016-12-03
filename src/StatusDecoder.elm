module StatusDecoder exposing (..)

import StatusTypes exposing (..)
import Json.Decode exposing (..)


serverDecoder : Decoder ServerRec
serverDecoder =
    map2 ServerRec
        (field "up" bool)
        (field "lastPing" (nullable string))


statusDecoder : Decoder StatusRec
statusDecoder =
    map2 StatusRec
        (field "server" serverDecoder)
        (field "invalidConfigGroups" (list string))
