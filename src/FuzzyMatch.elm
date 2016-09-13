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
        match keyword =
            Fuzzy.match [] [] needle keyword
                |> .score

        score =
            List.map match ((String.split " " hay.display) ++ [ hay.code ])
                |> List.minimum
                |> Maybe.withDefault
                    10000
    in
        ( score, hay )


match : String -> List DisplayRec -> List DisplayRec
match rawString list =
    let
        s =
            clean rawString
    in
        if String.isEmpty s then
            list
        else
            List.map (score s) list
                |> List.sortBy fst
                |> List.filter (((>) 1100) << fst)
                |> List.map snd
