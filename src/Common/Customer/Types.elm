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
    , smsOverride : Authorized
    , created : Date
    , status : Maybe String
    , authorizedOverride : Authorized
    , authorizedAt : Maybe Date
    , idCardData : Maybe String
    , idCardDataOverride : Authorized
    , idCardAt : Maybe Date
    , idCardImagePath : Maybe String
    , idCardPhotoOverride : Authorized
    , idCardImageAt : Maybe Date
    , sanctionsCheck : Maybe String
    , sanctionsCheckOverride : Authorized
    , sanctionsCheckAt : Maybe Date
    , frontFacingCamPath : Maybe String
    , frontFacingCamOverride : Authorized
    , frontFacingCamAt : Maybe Date
    }
