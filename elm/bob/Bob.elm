module Bob exposing (..)

type Phrase = Silence | Statement | Shout | Question

determine : String -> Phrase
determine str =
    Silence

response : Phrase -> String
response phrase =
    "."

hey : String -> String
hey request =
    response (determine request)
