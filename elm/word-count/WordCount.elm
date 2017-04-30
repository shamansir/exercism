module WordCount exposing (wordCount)

import Dict exposing (..)
import Regex as Rx exposing (..)

wordCount : String -> Dict String Int
wordCount phrase =
    List.foldl addWord Dict.empty (extractWords phrase)

addWord : String -> Dict String Int -> Dict String Int
addWord word dict =
    let
        lcWord = String.toLower word
    in
        if String.isEmpty lcWord then
            dict
        else
            case dict |> Dict.get lcWord of
                Nothing -> dict |> Dict.insert lcWord 1
                Just count -> dict |> Dict.insert lcWord (count + 1)

extractWords : String -> List String
extractWords source =
    source |> Rx.split Rx.All (Rx.regex  "[.\\s,!&:@$^%]+")
