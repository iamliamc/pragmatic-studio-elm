module Bingo exposing (..)

import Html


playerInfo name gameNumber =
    name ++ " - Game #" ++ (toString gameNumber)


playerInfoText name gameNumber =
    playerInfo name gameNumber
        |> String.toUpper
        |> Html.text


main =
    playerInfoText "Liam" 55



-- Example of Anonymous function
-- String.filter (\c -> c == 'e') "eieio"
-- isKeeper c = c == 'e'
-- String.filter isKeeper "eieio"
