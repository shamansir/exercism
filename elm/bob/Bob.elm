module Bob exposing (..)

import String
import Char

type Phrase
    = NotDetermined
    | Silence
    | Statement
    | Question
    | Shout

isSpace : Char -> Bool
isSpace char =
    (char == ' ') ||
    (char == '\t') ||
    (char == '\n')

isShoutingWord : String -> Bool
isShoutingWord word =
    (String.all Char.isUpper word) &&
    not (word == "OK") &&
    not (word == "DMV")

looksLikeShout : String -> Bool
looksLikeShout str =
    if String.endsWith "GO!" str then
        True
    else
        let
            wordsCount = List.length (String.words str)
            shoutWordsCount = List.length (List.filter isShoutingWord (String.words str))
        in
            (toFloat shoutWordsCount) >= ((toFloat wordsCount) / 2)

determine : String -> Phrase
determine str =
    if String.isEmpty str || String.all isSpace str then
        Silence
    else if looksLikeShout str then
        Shout
    else if String.endsWith "." str then
        Statement
    else if String.endsWith "?" str then
        Question
    else
        Statement

response : Phrase -> String
response phrase =
    case phrase of
        NotDetermined ->
            "My bad."
        Silence ->
            "Fine. Be that way!"
        Statement ->
            "Whatever."
        Question ->
            "Sure."
        Shout ->
            "Whoa, chill out!"

hey : String -> String
hey request =
    determine request
        |> response
