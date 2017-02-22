module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


-- type annotation


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


view : Html msg
view =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer "Nicole" 6
        , viewFooter
        ]


main =
    view



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
-- playerInfo : String -> Int -> String
