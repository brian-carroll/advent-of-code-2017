module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Parser exposing (Parser, (|=), (|.))
import Dict exposing (Dict)
import Char
import Task
import Array.Hamt as Array exposing (Array)


-- Types


type Msg
    = ParseInput String
    | Execute Int
    | Done ()


type alias Model =
    { registers : Registers
    , instructions : Array Instruction
    , parseError : Maybe Parser.Error
    , largestValue : Int
    , largestValueEver : Int
    }


type alias Registers =
    Dict String Int


type Direction
    = Inc
    | Dec


type alias Instruction =
    { updateReg : String
    , dir : Direction
    , amount : Int
    , condReg : String
    , condOp : Operator
    , condVal : Int
    }


type Operator
    = EQ
    | NE
    | GT
    | LT
    | GE
    | LE



-- Elm Architecture


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


init : ( Model, Cmd Msg )
init =
    ( { registers = Dict.empty
      , instructions = Array.empty
      , parseError = Nothing
      , largestValue = 0
      , largestValueEver = 0
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Day 8: I Heard You Like Registers" ]
        , textarea
            [ placeholder "Enter input here"
            , onInput ParseInput
            , rows 20
            , style
                [ ( "width", "800px" )
                , ( "padding", "10px" )
                , ( "margin", "10px" )
                ]
            ]
            []
        , viewError model.parseError
        , p [] [ text ("Largest value: " ++ (toString model.largestValue)) ]
        , p [] [ text ("Largest value ever: " ++ (toString model.largestValueEver)) ]
        , viewRegisters model.registers
        ]


viewError : Maybe Parser.Error -> Html msg
viewError maybeError =
    case maybeError of
        Nothing ->
            div [] []

        Just error ->
            p [ style [ ( "color", "red" ), ( "padding", "10px" ) ] ]
                [ p [] [ text ("Context: " ++ toString error.context) ]
                , p [] [ text ("Problem: " ++ toString error.problem) ]
                , viewErrorSource error
                ]


viewErrorSource : Parser.Error -> Html msg
viewErrorSource error =
    let
        marker =
            (String.repeat (error.col - 1) " ") ++ "^"

        lines =
            error.source
                |> String.lines

        sourceWithMarker =
            String.join "\n"
                ((List.take error.row lines)
                    ++ [ marker ]
                    ++ (List.drop error.row lines)
                )
    in
        pre [] [ text sourceWithMarker ]


viewRegisters : Dict String Int -> Html msg
viewRegisters registers =
    div []
        [ h3 [] [ text "Registers" ]
        , table []
            (Dict.foldr
                (\name value acc ->
                    (tr []
                        [ td [] [ text name ]
                        , td [] [ text (toString value) ]
                        ]
                    )
                        :: acc
                )
                []
                registers
            )
        ]


largestRegister : Registers -> Int
largestRegister registers =
    Dict.values registers
        |> List.maximum
        |> Maybe.withDefault 0



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ParseInput input ->
            ( parseInput input (Tuple.first init)
            , Task.perform Execute (Task.succeed 0)
            )

        Execute pc ->
            execute model pc

        Done _ ->
            ( model, Cmd.none )


parseInput : String -> Model -> Model
parseInput input model =
    case Parser.run inputParser input of
        Ok instructionList ->
            { model
                | instructions =
                    Array.fromList instructionList
            }

        Err e ->
            { model
                | parseError = Just e
            }



-- Execute instructions


execute : Model -> Int -> ( Model, Cmd Msg )
execute model pc =
    case Array.get pc model.instructions of
        Just instruction ->
            let
                registers =
                    executeInstruction instruction model.registers

                largestValue =
                    largestRegister registers
            in
                ( { model
                    | registers = registers
                    , largestValue = largestValue
                    , largestValueEver = Basics.max largestValue model.largestValueEver
                  }
                , Task.perform Execute (Task.succeed (pc + 1))
                )

        Nothing ->
            ( model
            , Task.perform Done (Task.succeed ())
            )


executeInstruction : Instruction -> Registers -> Registers
executeInstruction { updateReg, dir, amount, condReg, condOp, condVal } registers =
    let
        condRegVal =
            Dict.get condReg registers
                |> Maybe.withDefault 0

        isConditionTrue =
            case condOp of
                EQ ->
                    condRegVal == condVal

                NE ->
                    condRegVal /= condVal

                GT ->
                    condRegVal > condVal

                LT ->
                    condRegVal < condVal

                GE ->
                    condRegVal >= condVal

                LE ->
                    condRegVal <= condVal
    in
        if isConditionTrue then
            case dir of
                Inc ->
                    Dict.update
                        updateReg
                        (updateRegister amount)
                        registers

                Dec ->
                    Dict.update
                        updateReg
                        (updateRegister (negate amount))
                        registers
        else
            registers


updateRegister : Int -> Maybe Int -> Maybe Int
updateRegister increment maybeVal =
    Maybe.withDefault 0 maybeVal
        |> ((+) increment)
        |> Just



-- Parsers


regParser : Parser String
regParser =
    Parser.inContext "register name" <|
        Parser.keep Parser.oneOrMore Char.isLower


directionParser : Parser Direction
directionParser =
    Parser.inContext "direction" <|
        Parser.oneOf
            [ Parser.map (\_ -> Inc) (Parser.symbol "inc")
            , Parser.map (\_ -> Dec) (Parser.symbol "dec")
            ]


operatorParser : Parser Operator
operatorParser =
    Parser.inContext "operator" <|
        Parser.oneOf
            [ Parser.map (\_ -> EQ) (Parser.symbol "==")
            , Parser.map (\_ -> NE) (Parser.symbol "!=")
            , Parser.map (\_ -> GE) (Parser.symbol ">=")
            , Parser.map (\_ -> LE) (Parser.symbol "<=")
            , Parser.map (\_ -> GT) (Parser.symbol ">")
            , Parser.map (\_ -> LT) (Parser.symbol "<")
            ]


space : Parser ()
space =
    Parser.symbol " "


newline : Parser ()
newline =
    Parser.oneOf
        [ Parser.symbol "\n"
        , Parser.end
        ]


int : Parser Int
int =
    Parser.oneOf
        [ Parser.int
        , (Parser.succeed negate
            |. Parser.symbol "-"
            |= Parser.int
          )
        ]


instructionParser : Parser Instruction
instructionParser =
    Parser.succeed Instruction
        |= regParser
        |. space
        |= directionParser
        |. space
        |= int
        |. space
        |. Parser.symbol "if"
        |. space
        |= regParser
        |. space
        |= operatorParser
        |. space
        |= int
        |. newline


inputParser : Parser (List Instruction)
inputParser =
    Parser.repeat Parser.oneOrMore instructionParser
