module CoreTypes
    exposing
        ( Msg(..)
        , Category(..)
        , Route(..)
        , MachineSubRoute(..)
        , machineSubRouteToString
        )

import Navigation
import Pair
import Account
import Config
import Machine
import Transaction
import StatusTypes


type Category
    = AccountCat
    | ConfigCat
    | MachineCat


machineSubRouteToString : MachineSubRoute -> String
machineSubRouteToString machineSubRoute =
    case machineSubRoute of
        MachineActions ->
            "actions"


type MachineSubRoute
    = MachineActions


type Route
    = AccountRoute String
    | PairRoute
    | ConfigRoute String (Maybe String)
    | MachineRoute MachineSubRoute
    | TransactionRoute
    | NotFoundRoute


type Msg
    = AccountMsg Account.Msg
    | PairMsg Pair.Msg
    | ConfigMsg Config.Msg
    | MachineMsg Machine.Msg
    | TransactionMsg Transaction.Msg
    | LoadAccounts (List ( String, String ))
    | LoadStatus StatusTypes.WebStatus
    | NewUrl String
    | UrlChange Navigation.Location
    | Interval
