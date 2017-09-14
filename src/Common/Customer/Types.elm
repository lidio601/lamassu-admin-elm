module Common.Customer.Types exposing (..)

import Date exposing (Date)


type alias Customers =
    List Customer


type alias Customer =
    { id : String
    , name : Maybe String
    , phone : Maybe String
    , phoneAt : Maybe Date
    , created : Date
    , status : Maybe String
    , authorizedOverride : Maybe String
    }
