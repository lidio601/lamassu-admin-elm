module Customer.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import RemoteData exposing (..)
import Css.Admin as CSSAdmin exposing (..)
import Css.Classes as C
import Common.Customer.Types exposing (..)
import Customer.Types exposing (..)
import Date exposing (..)
import Date.Extra exposing (toFormattedString)


customerActions : String -> Authorized -> Html Msg
customerActions id authorizedOverride =
    case authorizedOverride of
        Blocked ->
            button [ onClick (PatchCustomer id "authorizedOverride" "verified") ] [ text "Unblock" ]

        Verified ->
            button [ onClick (PatchCustomer id "authorizedOverride" "blocked") ] [ text "Block" ]

        Automatic ->
            button [ onClick (PatchCustomer id "authorizedOverride" "automatic") ] [ text "Unblock" ]


formatDate : Maybe Date -> String
formatDate date =
    case date of
        Just date ->
            toFormattedString "yyyy-MM-dd HH:mm" date

        Nothing ->
            ""


maybeText : Maybe String -> String
maybeText maybeString =
    Maybe.withDefault "" maybeString


actions : String -> String -> Authorized -> Html Msg
actions id fieldKey checkedValue =
    (div []
        [ div []
            [ radio fieldKey checkedValue "automatic" (PatchCustomer id fieldKey (authorizedToString Automatic))
            , radio fieldKey checkedValue "blocked" (PatchCustomer id fieldKey (authorizedToString Blocked))
            , radio fieldKey checkedValue "verified" (PatchCustomer id fieldKey (authorizedToString Verified))
            ]
        ]
    )


radio : String -> Authorized -> String -> msg -> Html msg
radio inputName checkedValue value msg =
    label
        [ style [ ( "padding", "5px" ) ] ]
        [ input [ checked (authorizedToString checkedValue == value), type_ "radio", name inputName, onClick msg ] []
        , text value
        ]


verifyStatus : Maybe String -> Authorized -> Html Msg
verifyStatus complianceType fieldOverride =
    if fieldOverride == Verified || (complianceType /= Nothing && fieldOverride == Automatic) then
        text "Verified"
    else
        text "Unverified"


customerView : Customer -> Html Msg
customerView customer =
    div []
        [ h1 [] [ text "Customer Details" ]
        , table [ CSSAdmin.class [ C.TxTable ] ]
            [ tbody []
                [ tr []
                    [ td [] [ text "Customer ID" ]
                    , td [] [ text customer.id ]
                    ]
                , tr []
                    [ td [] [ text "Name" ]
                    , td [] [ text (maybeText customer.name) ]
                    ]
                , tr []
                    [ td [] [ text "Phone" ]
                    , td [] [ text (maybeText customer.phone) ]
                    ]
                , tr []
                    [ td [] [ text "Completed phone at" ]
                    , td [] [ text (formatDate customer.phoneAt) ]
                    ]
                , tr []
                    [ td [] [ text "Created" ]
                    , td [] [ text (toFormattedString "yyyy-MM-dd HH:mm" customer.created) ]
                    ]
                , tr []
                    [ td [] [ text "Block Customer" ]
                    , td []
                        [ customerActions customer.id customer.authorizedOverride ]
                    ]
                , tr []
                    [ td [] [ text "Authorized at " ]
                    , td [] [ text (formatDate customer.authorizedAt) ]
                    ]
                ]
            ]
        , h2 [] [ text "Compliance types" ]
        , table [ CSSAdmin.class [ C.TxTable ] ]
            [ thead []
                [ tr []
                    [ td [] [ text "Name" ]
                    , td [] [ text "Date" ]
                    , td [] [ text "Verify Status" ]
                    , td [] [ text "Override Status" ]
                    , td [] [ text "Actions" ]
                    ]
                ]
            , tbody []
                [ tr []
                    [ td [] [ text "SMS" ]
                    , td [] [ text (formatDate customer.phoneAt) ]
                    , td [] [ verifyStatus customer.phone customer.smsOverride ]
                    , td [] [ text (authorizedToString customer.smsOverride) ]
                    , td [] [ actions customer.id "smsOverride" customer.smsOverride ]
                    ]
                , tr []
                    [ td [] [ text "ID Card Data" ]
                    , td [] [ text (formatDate customer.idCardAt) ]
                    , td [] [ verifyStatus customer.idCardData customer.idCardDataOverride ]
                    , td [] [ text (authorizedToString customer.idCardDataOverride) ]
                    , td [] [ actions customer.id "idCardDataOverride" customer.idCardDataOverride ]
                    ]
                , tr []
                    [ td [] [ text "ID Card Photo" ]
                    , td [] [ text (formatDate customer.idCardImageAt) ]
                    , td [] [ verifyStatus customer.idCardImagePath customer.idCardPhotoOverride ]
                    , td [] [ text (authorizedToString customer.idCardPhotoOverride) ]
                    , td [] [ actions customer.id "idCardPhotoOverride" customer.idCardPhotoOverride ]
                    ]
                , tr []
                    [ td [] [ text "Front Facing Camera" ]
                    , td [] [ text (formatDate customer.frontFacingCamAt) ]
                    , td [] [ verifyStatus customer.frontFacingCamPath customer.frontFacingCamOverride ]
                    , td [] [ text (authorizedToString customer.frontFacingCamOverride) ]
                    , td [] [ actions customer.id "frontFacingCamOverride" customer.frontFacingCamOverride ]
                    ]
                , tr []
                    [ td [] [ text "Sanctions Check" ]
                    , td [] [ text (formatDate customer.sanctionsCheckAt) ]
                    , td [] [ verifyStatus customer.sanctionsCheck customer.sanctionsCheckOverride ]
                    , td [] [ text (authorizedToString customer.sanctionsCheckOverride) ]
                    , td [] [ actions customer.id "sanctionsCheckOverride" customer.sanctionsCheckOverride ]
                    ]
                ]
            ]
        ]


authorizedToString : Authorized -> String
authorizedToString model =
    case model of
        Verified ->
            "verified"

        Blocked ->
            "blocked"

        Automatic ->
            "automatic"


view : Model -> Html Msg
view model =
    case model of
        NotAsked ->
            div [] []

        Loading ->
            div [] [ text "Loading..." ]

        Failure err ->
            div [] [ text (toString err) ]

        Success customer ->
            div [] [ customerView customer ]
