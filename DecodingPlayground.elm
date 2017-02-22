module DecodingPlayground exposing (..)

import Html exposing (..)
import Json.Decode exposing (..)


json =
    """
    {
      "id": 1,
      "phrase": "Future-Proof",
      "points": 100
    }
    """


decoder =
    field "id" int


decodingResult =
    decodeString decoder json


view result =
    case result of
        Ok value ->
            text (toString value)

        Err err ->
            text (toString err)


main =
    view decodingResult
