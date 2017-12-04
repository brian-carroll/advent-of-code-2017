module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { input : Int
    , output : Result String Int
    }


type Msg
    = InputChanged String


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


initMessage : String
initMessage =
    "Please enter a number to start"


init : ( Model, Cmd Msg )
init =
    ( { input = 0
      , output = Err initMessage
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged inputString ->
            let
                nextModel =
                    case validate inputString of
                        Err errMsg ->
                            { input = 0
                            , output = Err errMsg
                            }

                        Ok input ->
                            { input = input
                            , output = Ok (solve input)
                            }
            in
                ( nextModel
                , Cmd.none
                )


validate : String -> Result String Int
validate input =
    if input == "" then
        Err initMessage
    else
        input
            |> String.trim
            |> String.toInt


validateRow : List String -> Result (List String) (List Int)
validateRow row =
    row
        |> List.map (String.toInt)
        |> List.foldr validateChar (Ok [])


validateChar : Result String Int -> Result (List String) (List Int) -> Result (List String) (List Int)
validateChar intResult acc =
    case ( intResult, acc ) of
        ( Err charMsg, Ok _ ) ->
            Err [ charMsg ]

        ( Err charMsg, Err strMsg ) ->
            Err (charMsg :: strMsg)

        ( Ok int, Err msg ) ->
            Err msg

        ( Ok int, Ok ints ) ->
            Ok (int :: ints)


type Direction
    = Right
    | Left
    | Up
    | Down


solve : Int -> Int
solve input =
    0


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Day 3: Spiral Memory" ]
        , textarea
            [ placeholder "Enter input here"
            , onInput InputChanged
            ]
            []
        , viewSolution model.output
        ]


viewSolution : Result String Int -> Html msg
viewSolution solution =
    case solution of
        Ok _ ->
            div []
                [ text "Solution: "
                , text <| toString <| Result.withDefault 0 solution
                ]

        Err errMsg ->
            [ errMsg ]
                |> List.map (\s -> li [] [ text s ])
                |> ul [ style [ ( "color", "red" ) ] ]
