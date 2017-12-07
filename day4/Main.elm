module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Task


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Validity
    = Valid
    | Invalid


type alias Solution =
    { part1 : List Validity
    , part2 : List Validity
    }


type alias Passphrase =
    List String


type alias Model =
    { input : List Passphrase
    , current : Passphrase
    , part1 : List Validity
    , part2 : List Validity
    }


type Msg
    = InputChanged String
    | Next Passphrase


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( { input = []
      , current = []
      , part1 = []
      , part2 = []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged inputString ->
            let
                passphrases =
                    inputString
                        |> String.lines
                        |> List.map String.words
            in
                case passphrases of
                    next :: rest ->
                        ( { model | input = rest }
                        , Task.perform Next (Task.succeed next)
                        )

                    [] ->
                        ( model, Cmd.none )

        Next passphrase ->
            ( { model
                | part1 = solvePart1 model.input
              }
            , Cmd.none
            )


solvePart1 : List Passphrase -> List Validity
solvePart1 input =
    List.map noDuplicates input


noDuplicates : List a -> Validity
noDuplicates passphrase =
    case passphrase of
        [] ->
            Valid

        h :: t ->
            if List.any ((==) h) t then
                Invalid
            else
                noDuplicates t


countValid : List Validity -> Int
countValid list =
    List.foldl
        (\current acc ->
            if current == Valid then
                acc + 1
            else
                acc
        )
        0
        list


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Day 4: High-Entropy Passphrases" ]
        , textarea
            [ placeholder "Enter input here"
            , onInput InputChanged
            ]
            []
        , div []
            [ text ("Valid entries: " ++ toString (countValid model.part1))
            ]
        , table []
            (List.map2 viewPassphrase
                model.input
                (solvePart1 model.input)
            )
        ]


viewPassphrase : Passphrase -> Validity -> Html msg
viewPassphrase passphrase validity =
    tr []
        [ td [] [ text <| String.join " " passphrase ]
        , td [] [ text <| toString validity ]
        ]
