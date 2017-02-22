module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


-- MODEL


initialModel =
    { name = "Liam"
    , gameNumber = 1
    , entries = initialEntries
    }


initialEntries =
    [ { id = 1, phrase = "Future-Proof", points = 100, marked = False }
    , { id = 2, phrase = "Doing Agile", points = 200, marked = False }
    ]



-- VIEW


playerInfo : String -> Int -> String
playerInfo name gameNumber =
    name ++ " - Game #" ++ (toString gameNumber)


viewPlayer : String -> Int -> Html msg
viewPlayer name gameNumber =
    let
        playerInfoText =
            playerInfo name gameNumber
                |> String.toUpper
                |> text
    in
        h2 [ id "info", class "classy" ]
            [ playerInfoText ]


viewHeader : String -> Html msg
viewHeader title =
    header []
        [ h1 [] [ text title ] ]


viewFooter : Html msg
viewFooter =
    footer []
        [ a [ href "http://elm-lang.org" ]
            [ text "Powered By Elm" ]
        ]



-- view : Html msg


view model =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model.name model.gameNumber
        , div [ class "debug" ] [ text (toString model) ]
        , viewFooter
        ]


main =
    view initialModel



-- Example of Anonymous function
-- String.filter (\c -> c == 'e') "eieio"
-- isKeeper c = c == 'e'
-- String.filter isKeeper "eieio"
-- Elm has Type Inference (it can infer based on types of actions,
-- feels dynamic but with benefits of compcile time type checking (not runtime))
-- greet name = "Hi, " ++ name
-- <function> : String -> String
-- half x = x / 2
-- <function> : Float -> Float
-- half x = x // 2
-- <function> : Int -> Int
-- Type annotations help make clear the contract of functions
-- When you call a function with the "incorrect number of arguments" it passes back partially applied function
-- all functions in elm are curried "don't need to pass all the arguments at once"
-- playerInfo : String -> Int -> String
-- threeTimes = String.repeat 3
-- threeTimes "ho"
-- "hohoho" : String
-- "WOW" |> String.repeat 3 |> String.pad 20 '*' ### both return partially applied functions and then use "WOW" as final argument
-- > names = ["Liam", "Patty", "Mooch"]
-- ["Liam","Patty","Mooch"] : List String
-- List.map
-- <function : (a -> b) -> List a -> List b
-- -- create partially applied function that satisifies the argument (a -> b) of List.map
-- starStudded = String.pad 10 '*'
-- <function : String - String
-- List.map starStudded names
-- ["***Liam***","***Patty**","***Mooch**"] : List String
