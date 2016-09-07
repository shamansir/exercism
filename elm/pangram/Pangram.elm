module Pangram exposing (..)

import String
import Char
import Set

isPangram : String -> Bool
isPangram str =
    (findLetterSum str) == (sumUpTo 26)

{- Converts "AbC0" -> "abc0" -> [ 97, 98, 99, 122 ] -> [ 97, 98, 99 ] -> [ 1, 2, 3 ] -> 6

   So converts string to char codes, removes non-letters, subtracts 96 from every
   code an then sums them up.
-}
findLetterSum : String -> Int
findLetterSum str =
    str
        |> String.toLower -- convert all chars in a string to lower case
        |> String.toList -- convert the result to a list of chars
        |> List.map Char.toCode -- map this list to a list of char codes
        |> List.filter isLetter -- leave only letter-codes (from 97 `a` to 122 `z`) in this list
        |> List.map subtractA -- sustract a code of letter `<'a'> - 1` (which is 97 - 1 = 96)
                              -- from every char code, so we get a list of numbers from 1 (`a`) to 26 (`z`)
        |> unique -- remove duplicates of the codes, so every number from 1 to 26
                  -- now appears (if ever appears) in the list only once
        |> List.sum -- sums all the numbers

{- Find a sum of all the numbers before n:

   sumUpTo 3 = 1 + 2 + 3 = 6
   sumUpTo 26 = 1 + 2 + 3 + 4 + ... + 25 + 26 = 351
-}
sumUpTo : Int -> Int
sumUpTo n =
    let
        nFloat = toFloat n
    in
        floor ((nFloat * (nFloat + 1)) / 2)

isLetter : Char.KeyCode -> Bool
isLetter code =
    (code >= 97) && (code <= 122)

subtractA : Int -> Int
subtractA code =
    code - 96



-- `unique` and `uniqueHelp` are copied from elm-community/list-extra

unique : List comparable -> List comparable
unique list =
  uniqueHelp identity Set.empty list

-- `unique` and `uniqueHelp` are copied from elm-community/list-extra

uniqueHelp : (a -> comparable) -> Set.Set comparable -> List a -> List a
uniqueHelp f existing remaining =
  case remaining of
    [] ->
      []

    first :: rest ->
      let computedFirst = f first in
      if Set.member computedFirst existing then
        uniqueHelp f existing rest
      else
        first :: uniqueHelp f (Set.insert computedFirst existing) rest
