module WordCount exposing (wordCount)

import Dict exposing (..)

wordCount : String -> Dict String Int
wordCount phrase =
    let
      words = phrase |> String.words
      result = Dict.empty
    in
      List.foldl addWord result words

addWord : String -> Dict String Int -> Dict String Int
addWord word dict =
    case dict |> Dict.get word of
        Nothing -> dict |> Dict.insert word 1
        Just count -> dict |> Dict.insert word (count + 1)
