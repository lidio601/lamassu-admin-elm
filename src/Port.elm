port module Port exposing (qr)


type alias QrRec =
    { id : String, content : String }


qr : String -> String -> Cmd a
qr id content =
    portQr { id = id, content = content }


port portQr : QrRec -> Cmd msg
