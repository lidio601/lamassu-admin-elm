module CoreTypes
    exposing
        ( Msg(..)
        , Category(..)
        , Route(..)
        )

import Navigation
import Pair
import Account
import Config
import MaintenanceMachines.Types
import MaintenanceFunding.Types
import Transaction
import StatusTypes


type Category
    = AccountCat
    | MachineSettingsCat
    | GlobalSettingsCat
    | MaintenanceCat


type Route
    = AccountRoute String
    | PairRoute
    | ConfigRoute String (Maybe String)
    | TransactionRoute
    | MaintenanceMachinesRoute
    | MaintenanceFundingRoute (Maybe String)
    | NotFoundRoute


type Msg
    = AccountMsg Account.Msg
    | PairMsg Pair.Msg
    | ConfigMsg Config.Msg
    | MaintenanceMachinesMsg MaintenanceMachines.Types.Msg
    | MaintenanceFundingMsg MaintenanceFunding.Types.Msg
    | TransactionMsg Transaction.Msg
    | LoadAccounts (List ( String, String ))
    | LoadStatus StatusTypes.WebStatus
    | NewUrl String
    | UrlChange Navigation.Location
    | Interval
    | WebSocketMsg String
