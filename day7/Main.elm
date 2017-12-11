module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Parser exposing (..)
import Parser.LanguageKit as Parser exposing (..)
import Char
import Dict exposing (Dict)
import Task


-- Types


type alias Prog =
    { name : String
    , weight : Maybe Int
    , children : Children
    }


type Children
    = Children (List Prog)


type alias Model =
    { parsedLines : Dict String Prog
    , parseErrors : List Parser.Error
    , tree : Maybe Prog
    , imbalance : Maybe IterationResult
    }


type Msg
    = ParseInput String
    | BuildTree ()
    | FindImbalance ()
    | Done ()



-- Elm Arch


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
    ( { parsedLines = Dict.empty
      , parseErrors = []
      , tree = Nothing
      , imbalance = Nothing
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Day 7: Recursive Circus" ]
        , textarea
            [ placeholder "Enter input here"
            , onInput ParseInput
            , rows 20
            , style [ ( "width", "800px" ) ]
            ]
            []
        , viewParseErrors model.parseErrors
        , viewImbalance model.imbalance
        , h2 [] [ text "Tree" ]
        , viewTree model.tree
        ]


viewImbalance : Maybe IterationResult -> Html msg
viewImbalance imbalance =
    case imbalance of
        Just (Imbalance badProg correctWeight) ->
            div []
                [ h2 [] [ text "Imbalanced program" ]
                , p [] [ text ("Program " ++ badProg.name) ]
                , p [] [ text ("Actual weight: " ++ toString (ownWeight badProg)) ]
                , p [] [ text ("Correct weight: " ++ toString correctWeight) ]
                ]

        _ ->
            div [] []


viewTree : Maybe Prog -> Html msg
viewTree tree =
    case tree of
        Just root ->
            ul [] [ viewProg root ]

        Nothing ->
            div [] []


viewProg : Prog -> Html msg
viewProg prog =
    li []
        ([ text prog.name ]
            ++ viewChildren prog.children
        )


viewChildren : Children -> List (Html msg)
viewChildren (Children progs) =
    case progs of
        [] ->
            []

        _ ->
            [ ul [] (List.map viewProg progs)
            ]


viewParseErrors : List Error -> Html msg
viewParseErrors errors =
    ul [ style [ ( "color", "red" ) ] ]
        (List.map
            (\errMsg ->
                li [] [ text (toString errMsg) ]
            )
            errors
        )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ParseInput input ->
            ( parseInput input (Tuple.first init)
            , Task.perform BuildTree (Task.succeed ())
            )

        BuildTree _ ->
            ( { model
                | tree = buildTree model.parsedLines
              }
            , Task.perform FindImbalance (Task.succeed ())
            )

        FindImbalance _ ->
            ( { model
                | imbalance = Maybe.map findImbalance model.tree
              }
            , Task.perform Done (Task.succeed ())
            )

        Done _ ->
            ( model, Cmd.none )


parseInput : String -> Model -> Model
parseInput input model =
    case run inputParser input of
        Ok progList ->
            { model
                | parsedLines =
                    List.foldl
                        (\p acc -> Dict.insert p.name p acc)
                        Dict.empty
                        progList
            }

        Err e ->
            { model | parseErrors = [ e ] }


type IterationResult
    = SubTreeWeight Int
    | Imbalance Prog Int


isImbalance : IterationResult -> Bool
isImbalance iterationResult =
    case iterationResult of
        Imbalance badProg extraWeight ->
            True

        SubTreeWeight w ->
            False


getSubTreeWeights : List IterationResult -> List Int
getSubTreeWeights iterationResults =
    List.filterMap
        (\ir ->
            case ir of
                Imbalance _ _ ->
                    Nothing

                SubTreeWeight w ->
                    Just w
        )
        iterationResults


findImbalance : Prog -> IterationResult
findImbalance prog =
    case prog.children of
        Children childList ->
            let
                iterationResults =
                    List.map findImbalance childList
            in
                case List.filter isImbalance iterationResults of
                    alreadyFound :: _ ->
                        alreadyFound

                    [] ->
                        let
                            -- We know all iterationResults are SubTreeWeights, but the type checker doesn't!
                            -- we need to filter (and unwrap) them
                            subTreeWeights =
                                getSubTreeWeights iterationResults

                            progsAndSubTreeWeights =
                                List.map2 (,) childList subTreeWeights

                            mostCommonWeight =
                                mostCommonValue subTreeWeights

                            imbalancedProgs =
                                List.filter
                                    -- next line is wrong, it's not ownWeight, it's subtreeweight
                                    (\( child, stw ) -> stw /= mostCommonWeight)
                                    progsAndSubTreeWeights
                        in
                            case imbalancedProgs of
                                [] ->
                                    -- next line is wrong, it's not ownWeight, it's subtreeweight
                                    SubTreeWeight (ownWeight prog + List.sum subTreeWeights)

                                ( badProg, stw ) :: _ ->
                                    let
                                        correctWeight =
                                            (ownWeight badProg) - (stw - mostCommonWeight)
                                    in
                                        Imbalance badProg correctWeight


mostCommonValue : List Int -> Int
mostCommonValue list =
    let
        histogram =
            List.foldl
                (\num -> Dict.update num incrementFreq)
                Dict.empty
                list

        ( mostCommonVal, _ ) =
            Dict.foldl
                (\val freq ( accVal, accFreq ) ->
                    if freq > accFreq then
                        ( val, freq )
                    else
                        ( accVal, accFreq )
                )
                ( 0, 0 )
                histogram
    in
        mostCommonVal


incrementFreq : Maybe Int -> Maybe Int
incrementFreq maybeCount =
    Maybe.withDefault 0 maybeCount
        |> ((+) 1)
        |> Just


totalWeight : Prog -> Int
totalWeight tree =
    ownWeight tree + List.sum (childWeights tree.children)


childWeights : Children -> List Int
childWeights (Children childList) =
    List.map totalWeight childList


ownWeight : Prog -> Int
ownWeight prog =
    Maybe.withDefault 0 prog.weight


{-| Remove children from the flat dict and put them into the tree structure
fold over all dict keys
if has no children, skip
if has children
recursively move descendants from dict to parent
-}
buildTree : Dict String Prog -> Maybe Prog
buildTree dict =
    let
        newDict =
            Dict.foldl
                (\key value acc ->
                    moveChildrenToParent key acc
                )
                dict
                dict
    in
        Dict.values newDict
            |> List.head


moveChildrenToParent : String -> Dict String Prog -> Dict String Prog
moveChildrenToParent parentName dict =
    case Dict.get parentName dict of
        Nothing ->
            dict

        Just parent ->
            case parent.children of
                Children childList ->
                    let
                        ( newChildList, smallerDict ) =
                            List.foldr
                                (\child ( accChildList, accDict ) ->
                                    let
                                        dictWithoutGrandkids =
                                            moveChildrenToParent child.name accDict

                                        childTree =
                                            Dict.get child.name dictWithoutGrandkids
                                                |> Maybe.withDefault child
                                    in
                                        ( childTree :: accChildList
                                        , Dict.remove child.name dictWithoutGrandkids
                                        )
                                )
                                ( [], dict )
                                childList
                    in
                        Dict.insert parent.name
                            { parent | children = Children newChildList }
                            smallerDict



-- Parsers


spaces : Parser ()
spaces =
    ignore zeroOrMore (\char -> char == ' ')


newline : Parser ()
newline =
    ignore oneOrMore (\char -> char == '\n')


nameParser : Parser String
nameParser =
    keep oneOrMore Char.isLower


weightParser : Parser Int
weightParser =
    succeed identity
        |. symbol "("
        |= int
        |. symbol ")"


childParser : Parser Prog
childParser =
    Parser.map
        (\name ->
            { name = name
            , weight = Nothing
            , children = Children []
            }
        )
        nameParser


childrenParser : Parser Children
childrenParser =
    inContext "children" <|
        Parser.map Children <|
            sequence
                { start = "->"
                , separator = ","
                , end = ""
                , spaces = spaces
                , item = childParser
                , trailing = Forbidden
                }


progParser : Parser Prog
progParser =
    inContext "prog" <|
        succeed Prog
            |= nameParser
            |. spaces
            |= Parser.map Just weightParser
            |. spaces
            |= oneOf
                [ childrenParser
                , succeed (Children [])
                ]
            |. newline


inputParser : Parser (List Prog)
inputParser =
    repeat oneOrMore progParser


{-| Example for REPL debugging
-}
exampleInput : String
exampleInput =
    """pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)
"""
