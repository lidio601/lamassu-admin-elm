module SelectizeHelpers exposing (..)

import Selectize
import ConfigTypes exposing (..)
import String


maybeToList : Maybe a -> List a
maybeToList maybe =
    case maybe of
        Nothing ->
            []

        Just x ->
            [ x ]


selectizeItem : DisplayRec -> Selectize.Item String
selectizeItem displayRec =
    let
        code =
            displayRec.code

        searchWords =
            code :: (String.split " " displayRec.display)
    in
        Selectize.selectizeItem code code displayRec.display searchWords


initAccountSelectize : ConfigData -> String -> FieldScope -> Maybe String -> SelectizeModel
initAccountSelectize configData accountClass fieldScope maybeValue =
    let
        matches accountRec =
            (accountClass
                == accountRec.class
            )
                && case accountRec.cryptos of
                    Nothing ->
                        True

                    Just cryptos ->
                        List.member (Debug.log "DEBUG1 fieldScope" fieldScope.crypto) (Debug.log "DEBUG20" cryptos)

        toItem accountRec =
            if matches accountRec then
                Just
                    (Selectize.selectizeItem accountRec.code
                        accountRec.display
                        accountRec.display
                        [ accountRec.code ]
                    )
            else
                Nothing

        availableItems =
            List.filterMap toItem configData.accounts

        selectedCodes =
            maybeToList maybeValue
    in
        Selectize.init 1 5 selectedCodes availableItems


initCurrencySelectize : ConfigData -> FieldScope -> Maybe String -> SelectizeModel
initCurrencySelectize configData fieldScope maybeCurrency =
    let
        currencies =
            configData.currencies

        availableItems =
            List.map selectizeItem currencies

        selectedCodes =
            maybeToList maybeCurrency
    in
        Selectize.init 1 5 selectedCodes availableItems


initLanguageSelectize : ConfigData -> FieldScope -> Maybe (List String) -> SelectizeModel
initLanguageSelectize configData fieldScope maybeLanguages =
    let
        languages =
            configData.languages

        selectize language =
            Selectize.selectizeItem language.code
                language.code
                language.display
                (String.split " " language.display)

        availableItems =
            List.map selectize languages

        selectedCodes =
            Maybe.withDefault [] maybeLanguages
    in
        Selectize.init 4 5 selectedCodes availableItems
