module AccountParser exposing (..)

import AccountDecoder
import AccountEncoder
import AccountRecord


encode : AccountRecord.Account -> String
encode =
    AccountEncoder.encode


decode : String -> AccountRecord.AccountResult
decode =
    AccountDecoder.decode
