module AccountRecordTests exposing (..)

import Test exposing (..)
import Expect
import AccountRecord
import Result


testString : String
testString =
    """
    {
      "code": "twilio",
      "display": "Twilio",
      "fields": [
        {
          "code": "accountSid",
          "display": "Account SID",
          "type": "string",
          "secret": false,
          "required": true,
          "value": {
            "fieldType": "string",
            "value": "xx123"
          }
        }
      ]
    }
    """


testRecord : AccountRecord.Account
testRecord =
    { code = "twilio"
    , display = "Twilio"
    , fields =
        [ { code = "accountSid"
          , display = "Account SID"
          , secret = False
          , required = True
          , value = AccountRecord.FieldString "xx123"
          }
        ]
    }


all : Test
all =
    describe "Parse InitialRecord"
        [ test "Basic record" <|
            \() ->
                let
                    parsed =
                        AccountRecord.decode testString
                in
                    Expect.equal parsed (Ok testRecord)
        ]
