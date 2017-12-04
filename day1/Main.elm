module Main exposing (main)

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
    { input : List Int
    , solution : Result (List String) Int
    }


type Msg
    = InputChanged String


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


initMessage : String
initMessage =
    "Please enter numbers to start"


init : ( Model, Cmd Msg )
init =
    ( { input = []
      , solution = Err [ initMessage ]
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
                        Err errMsgs ->
                            { input = []
                            , solution = Err errMsgs
                            }

                        Ok intList ->
                            { input = intList
                            , solution = Ok (solve intList)
                            }
            in
                ( nextModel
                , Cmd.none
                )


validate : String -> Result (List String) (List Int)
validate input =
    if input == "" then
        Err [ initMessage ]
    else
        input
            |> String.split ""
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


solve : List Int -> Int
solve intList =
    let
        halfLength =
            (List.length intList) // 2

        firstHalf =
            List.take halfLength intList

        secondHalf =
            List.drop halfLength intList

        matchList =
            secondHalf ++ firstHalf
    in
        List.map2
            (\a b ->
                if a == b then
                    a
                else
                    0
            )
            intList
            matchList
            |> List.sum


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Day 1: Inverse Captcha" ]
        , textarea
            [ placeholder "Enter captcha here"
            , onInput InputChanged
            ]
            []
        , viewSolution model.solution
        ]


viewSolution : Result (List String) Int -> Html msg
viewSolution solution =
    case solution of
        Ok _ ->
            div []
                [ text "Solution: "
                , text <| toString <| Result.withDefault 0 solution
                ]

        Err errMsgs ->
            errMsgs
                |> List.map (\s -> li [] [ text s ])
                |> ul [ style [ ( "color", "red" ) ] ]
