module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Task
import Set


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
                ( { model
                    | input = passphrases
                    , part1 = solvePart1 passphrases
                    , part2 = solvePart2 passphrases
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


solvePart2 : List Passphrase -> List Validity
solvePart2 input =
    List.map noAnagrams input


noAnagrams : Passphrase -> Validity
noAnagrams passphrase =
    passphrase
        |> List.map (String.toList >> Set.fromList)
        |> noDuplicates


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
        , p []
            [ text ("Part 1: # Passphrases without duplicates: " ++ toString (countValid model.part1))
            ]
        , p []
            [ text ("Part 2: # Passphrases without anagrams: " ++ toString (countValid model.part2))
            ]
        , table []
            (tr []
                [ th [] [ text "Passphrase" ]
                , th [] [ text "Part 1" ]
                , th [] [ text "Part 2" ]
                ]
                :: List.map3 viewPassphrase
                    model.input
                    model.part1
                    model.part2
            )
        ]


viewPassphrase : Passphrase -> Validity -> Validity -> Html msg
viewPassphrase passphrase validity1 validity2 =
    tr []
        [ td [] [ text <| String.join " " passphrase ]
        , td [] [ text <| toString validity1 ]
        , td [] [ text <| toString validity2 ]
        ]
