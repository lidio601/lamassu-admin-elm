module InitialRecordTests exposing (..)

import Test exposing (..)
import Expect
import InitialRecord
import Result


testString : String
testString =
    """
{"config":{"global":{"global":{"plugins":[{"code":"twilio","display":"Twilio","fields":[{"code":"accountSid","display":"Account SID","type":"string","secret":false,"required":true}]}]}}}}
"""


testRecord : InitialRecord.InitialRecord
testRecord =
    { config =
        { global =
            { global =
                { plugins =
                    [ { code = "twilio"
                      , display = "Twilio"
                      , fields =
                            [ { code = "accountSid"
                              , display = "Account SID"
                              , fieldType = InitialRecord.FieldString
                              , secret = False
                              , required = True
                              }
                            ]
                      }
                    ]
                }
            }
        }
    }


all : Test
all =
    describe "Parse InitialRecord"
        [ test "Basic record" <|
            \() ->
                let
                    parsed =
                        InitialRecord.decode testString
                in
                    Expect.equal parsed (Ok testRecord)
        ]
