module FuzzyMatch exposing (match)

import String
import Fuzzy


clean : String -> String
clean s =
    String.trim s
        |> String.toLower


type alias DisplayRec =
    { code : String
    , display : String
    }


score : String -> DisplayRec -> ( Int, DisplayRec )
score needle hay =
    let
        cleanNeedle =
            clean needle

        match keyword =
            Fuzzy.match [] [] cleanNeedle keyword
                |> .score

        score =
            List.map match ((String.split " " hay.display) ++ [ hay.code ])
                |> List.minimum
                |> Maybe.withDefault
                    10000
    in
        ( score, hay )


match : String -> List DisplayRec -> List DisplayRec
match s list =
    List.map (score s) list
        |> List.sortBy fst
        |> List.filter (((>) 1100) << fst)
        |> List.map snd
