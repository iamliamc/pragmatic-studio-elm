module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- MODEL


type alias Model =
    { name : String
    , gameNumber : Int
    , entries : List Entry
    }


type alias Entry =
    { id : Int
    , phrase : String
    , points : Int
    , marked : Bool
    }


initialModel : Model
initialModel =
    { name = "Liam"
    , gameNumber = 2
    , entries = initialEntries
    }


initialEntries : List Entry
initialEntries =
    [ Entry 1 "Future-Proof" 100 False
    , Entry 2 "Doing Agile" 200 False
    , Entry 3 "In The Cloud" 300 False
    , Entry 4 "Rock-Star Ninja" 400 False
    ]



-- UPDATE
-- defines a union type enumerates possible values


type Msg
    = NewGame



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


viewEntryItem : Entry -> Html msg
viewEntryItem entry =
    li []
        [ span [ class "phrase" ] [ text entry.phrase ]
        , span [ class "points" ] [ text (toString entry.points) ]
        ]


viewEntryList : List Entry -> Html msg
viewEntryList entries =
    let
        listOfEntries =
            List.map viewEntryItem entries
    in
        ul [] listOfEntries


view : Model -> Html msg
view model =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model.name model.gameNumber
        , viewEntryList model.entries
        , div [ class "button-group" NewGame ]
            [ button [ onClick ] [ text "New Game" ] ]
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
-- Type aliases are essentially constructors for objects
-- In Elm anytime a user interacts with the page a 'message/actions/events' is generated
