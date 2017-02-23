module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Random
import Http
import ViewHelpers exposing (..)
import Entry
import Score


-- MODEL


type GameState
    = EnteringName
    | Playing


type alias Model =
    { name : String
    , gameNumber : Int
    , entries : List Entry.Entry
    , alertMessage : Maybe String
    , nameInput : String
    , gameState : GameState
    }


initialModel : Model
initialModel =
    { name = "Anonymous"
    , gameNumber = 1
    , entries = []
    , alertMessage = Nothing
    , nameInput = ""
    , gameState = EnteringName
    }



-- UPDATE
-- defines a union type enumerates possible values


type Msg
    = NewGame
    | Mark Int
    | NewRandom Int
    | Sort
    | NewEntries (Result Http.Error (List Entry.Entry))
    | CloseAlert
    | ShareScore
    | NewScore (Result Http.Error Score.Score)
    | SetNameInput String
    | SaveName
    | CancelName
    | ChangeGameState GameState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeGameState state ->
            ( { model | gameState = state }, Cmd.none )

        SaveName ->
            if (String.isEmpty model.nameInput) then
                ( { model | alertMessage = Just "Please Enter a Name" }, Cmd.none )
            else
                ( { model
                    | name = model.nameInput
                    , nameInput = ""
                    , gameState = Playing
                  }
                , Cmd.none
                )

        CancelName ->
            ( { model | nameInput = "", gameState = Playing }, Cmd.none )

        SetNameInput value ->
            ( { model | nameInput = value }, Cmd.none )

        NewRandom randomNumber ->
            ( { model | gameNumber = randomNumber }, Cmd.none )

        ShareScore ->
            ( model, postScore model )

        NewScore (Ok score) ->
            let
                message =
                    "Your score of "
                        ++ (toString score.score)
                        ++ " was successfully shared!"
            in
                ( { model | alertMessage = Just message }, Cmd.none )

        NewScore (Err error) ->
            ( { model | alertMessage = Just (httpErrorToMessage error) }, Cmd.none )

        NewGame ->
            ( { model | gameNumber = model.gameNumber + 1 }, getEntries )

        NewEntries (Ok randomEntries) ->
            ( { model | entries = randomEntries }, Cmd.none )

        NewEntries (Err error) ->
            ( { model | alertMessage = Just (httpErrorToMessage error) }, Cmd.none )

        Mark id ->
            ( { model | entries = Entry.markEntryWithId model.entries id }, Cmd.none )

        Sort ->
            ( { model | entries = List.sortBy .points model.entries }, Cmd.none )

        CloseAlert ->
            ( { model | alertMessage = Nothing }, Cmd.none )


httpErrorToMessage : Http.Error -> String
httpErrorToMessage error =
    case error of
        Http.NetworkError ->
            "Is the server running?"

        Http.BadStatus response ->
            case response.status.code of
                401 ->
                    "Unauthorized"

                404 ->
                    "Not Found"

                code ->
                    (toString code)

        Http.BadPayload message _ ->
            "Decoding Failed: " ++ message

        _ ->
            (toString error)



-- DECODERS / ENCODERS
-- See Score.elm & Entry.elm
-- COMMANDS


apiUrlPrefix : String
apiUrlPrefix =
    "http://localhost:3000"


generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate NewRandom (Random.int 1 100)


entriesUrl : String
entriesUrl =
    apiUrlPrefix ++ "/random-entries"


postScore : Model -> Cmd Msg
postScore model =
    Score.postScore NewScore apiUrlPrefix model.name (Entry.sumMarkedPoints model.entries)


getEntries : Cmd Msg
getEntries =
    Entry.getEntries NewEntries entriesUrl



-- VIEW


viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
    h2 [ id "info", class "classy" ]
        [ a [ href "#", onClick (ChangeGameState EnteringName) ] [ text name ]
        , text (" - Game #" ++ (toString gameNumber))
        ]


viewHeader : String -> Html Msg
viewHeader title =
    header []
        [ h1 [] [ text title ] ]


viewFooter : Html Msg
viewFooter =
    footer []
        [ a [ href "http://elm-lang.org" ]
            [ text "Powered By Elm" ]
        ]



-- .marked is the same as an anonymous function (\e -> e.marked) that access the values of marked filed takes an entry returns a boolean


allEntriesMarked : List Entry.Entry -> Bool
allEntriesMarked entries =
    List.all .marked entries


zeroScore : Model -> Bool
zeroScore model =
    (Entry.sumMarkedPoints model.entries) == 0


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO!"
        , viewPlayer model.name model.gameNumber
        , viewAlertMessage CloseAlert model.alertMessage
        , viewNameInput model
        , Entry.viewEntryList Mark model.entries
        , Score.viewScore (Entry.sumMarkedPoints model.entries)
        , div [ class "button-group" ]
            [ primaryButton NewGame "New Game"
            , primaryButton Sort "Sort"
            , button [ onClick ShareScore, disabled (zeroScore model) ] [ text "Share Score" ]
            ]
        , div [ class "debug" ] [ text (toString model) ]
        , viewFooter
        ]


viewNameInput : Model -> Html Msg
viewNameInput model =
    case model.gameState of
        EnteringName ->
            div [ class "name-input" ]
                [ input
                    [ type_ "text"
                    , placeholder "Who's Playing"
                    , value model.nameInput
                    , autofocus True
                    , onInput SetNameInput
                    ]
                    []
                , primaryButton SaveName "Save"
                , primaryButton CancelName "Cancel"
                ]

        Playing ->
            text ""


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, getEntries )
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }
