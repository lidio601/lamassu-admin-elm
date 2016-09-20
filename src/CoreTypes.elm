module CoreTypes exposing (Msg(..), Category(..), Page(..))

import Pair
import Account
import Config


type Category
    = AccountCat
    | ConfigCat


type Page
    = AccountPage String
    | PairPage
    | ConfigPage String (Maybe String)
    | UnknownPage


type Msg
    = AccountMsg Account.Msg
    | PairMsg Pair.Msg
    | ConfigMsg Config.Msg
    | NewPage (Maybe Category) Page
