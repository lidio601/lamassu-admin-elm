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
import Maintenance.Types
import Transaction
import StatusTypes


type Category
    = AccountCat
    | MaintenanceCat
    | MachineSettingsCat
    | GlobalSettingsCat


type Route
    = AccountRoute String
    | PairRoute
    | ConfigRoute String (Maybe String)
    | TransactionRoute
    | MaintenanceRoute String
    | NotFoundRoute


type Msg
    = AccountMsg Account.Msg
    | PairMsg Pair.Msg
    | ConfigMsg Config.Msg
    | MaintenanceMsg Maintenance.Types.Msg
    | TransactionMsg Transaction.Msg
    | LoadAccounts (List ( String, String ))
    | LoadStatus StatusTypes.WebStatus
    | NewUrl String
    | UrlChange Navigation.Location
    | Interval
    | WebSocketMsg String
