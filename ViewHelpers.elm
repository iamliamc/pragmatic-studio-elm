module ViewHelpers exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- type variables Msg is the uniontype for all updates in Bingo


primaryButton : msg -> String -> Html msg
primaryButton msg name =
    button [ class "primary", onClick msg ] [ text name ]


viewAlertMessage : msg -> Maybe String -> Html msg
viewAlertMessage msg alertMessage =
    case alertMessage of
        Just message ->
            div [ class "alert" ]
                [ span [ class "close", onClick msg ] [ text "X" ]
                , text message
                ]

        Nothing ->
            text ""
