module Anagram exposing (..)

import String

sortChars: String -> String
sortChars source =
    source
        |> String.toLower
        |> String.toList
        |> List.sort
        |> String.fromList

isAnagram: String -> String -> Maybe String
isAnagram subject sample =
    if String.toLower subject ==
       String.toLower sample then
        Nothing
    else if sortChars subject ==
            sortChars sample then
        Just sample
    else
        Nothing

detect : String -> List String -> List String
detect subject samples =
    List.filterMap (isAnagram subject) samples
