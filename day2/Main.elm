module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Result.Extra


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { spreadsheet : List (List Int)
    , checksum : Result (List String) Int
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
    ( { spreadsheet = [ [] ]
      , checksum = Err [ initMessage ]
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
                            { spreadsheet = [ [] ]
                            , checksum = Err errMsgs
                            }

                        Ok spreadsheet ->
                            { spreadsheet = spreadsheet
                            , checksum = Ok (solve spreadsheet)
                            }
            in
                ( nextModel
                , Cmd.none
                )


validate : String -> Result (List String) (List (List Int))
validate input =
    if input == "" then
        Err [ initMessage ]
    else
        input
            |> String.split "\n"
            |> List.map String.words
            |> List.map validateRow
            |> Result.Extra.combine


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


solve : List (List Int) -> Int
solve spreadsheet =
    spreadsheet
        |> List.map rowChecksum
        |> List.sum


rowChecksum : List Int -> Int
rowChecksum row =
    Debug.log "rowChecksum" <|
        List.sum <|
            List.indexedMap
                (\i1 n1 ->
                    List.sum <|
                        List.indexedMap
                            (\i2 n2 ->
                                if i1 == i2 then
                                    0
                                else if n1 % n2 /= 0 then
                                    0
                                else
                                    n1 // n2
                            )
                            row
                )
                row


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Day 2: Corruption Checksum" ]
        , textarea
            [ placeholder "Enter captcha here"
            , onInput InputChanged
            ]
            []
        , viewSolution model.checksum
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
