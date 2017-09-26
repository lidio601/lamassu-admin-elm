module Common.Customer.Types exposing (..)

import Date exposing (Date)


type Authorized
    = Automatic
    | Blocked
    | Verified


type alias Customers =
    List Customer


type alias Customer =
    { id : String
    , name : Maybe String
    , phone : Maybe String
    , phoneAt : Maybe Date
    , created : Date
    , status : Maybe String
    , authorizedOverride : Authorized
    , authorizedAt : Maybe Date
    }
