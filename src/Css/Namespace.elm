module Css.Namespace exposing (..)


name : String
name =
    "lamassuAdmin"


helpers : Html.CssHelpers.Namespace String class id msg
helpers =
    Html.CssHelpers.withNamespace name


className : class -> String
className class =
    Css.Helpers.identifierToString name class


class : List class -> Html.Attribute msg
class =
    helpers.class


id : id -> Html.Attribute msg
id =
    helpers.id
