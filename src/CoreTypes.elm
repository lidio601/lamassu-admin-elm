module CoreTypes exposing (Msg(..), Category(..), Route(..))

import Pair
import Account
import Config


type Category
    = AccountCat
    | ConfigCat


type Route
    = AccountRoute String
    | PairRoute
    | ConfigRoute String (Maybe String)
    | NotFoundRoute


type Msg
    = AccountMsg Account.Msg
    | PairMsg Pair.Msg
    | ConfigMsg Config.Msg
    | NewRoute (Maybe Category) Route
