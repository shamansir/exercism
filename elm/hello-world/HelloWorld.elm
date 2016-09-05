module HelloWorld exposing (helloWorld)

helloWorld: Maybe String -> String
helloWorld subject =
    case subject of

        Nothing ->
            "Hello, World!"

        Just name ->
            "Hello, " ++ name ++ "!"
