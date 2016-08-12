module GlobalGlobalConfig exposing (..)

-- MODEL


type alias Model =
    { config : Config
    }


getCon : Cmd Msg


getTotem =
    Http.getString "http://localhost:8093/totem"
        |> RemoteData.asCmd
        |> Cmd.map Totem


init : ( Model, Cmd Msg )
init =
    ( { totem = Nothing }, getTotem )



-- UPDATE


type Msg
    = Totem (WebData String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "DEBUG0" msg of
        Totem webdata ->
            case webdata of
                RemoteData.NotAsked ->
                    model ! []

                RemoteData.Loading ->
                    model ! []

                RemoteData.Failure _ ->
                    model ! []

                RemoteData.Success totem ->
                    model ! [ Port.qr "qr" (Debug.log "DEBUG1" totem) ]


view : Model -> Html Msg
view model =
    Html.canvas [ id "qr" ] []
