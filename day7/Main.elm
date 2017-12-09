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
    , tree : List Prog
    }


type Msg
    = ParseInput String
    | BuildTree ()
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
      , tree = []
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
        , viewTree model.tree
        ]


viewTree : List Prog -> Html msg
viewTree tree =
    ul [] (List.map viewProg tree)


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
            ( parseInput input model
            , Task.perform BuildTree (Task.succeed ())
            )

        BuildTree _ ->
            ( { model
                | tree = buildTree model.parsedLines
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


{-| Remove children from the flat dict and put them into the tree structure
fold over all dict keys
if has no children, skip
if has children
recursively
move descendants from dict to parent
-}
buildTree : Dict String Prog -> List Prog
buildTree dict =
    let
        newDict =
            Dict.foldl
                moveChildrenToParent
                dict
                dict
    in
        Dict.values newDict


moveChildrenToParent : a -> Prog -> Dict String Prog -> Dict String Prog
moveChildrenToParent _ parent dict =
    case parent.children of
        Children childList ->
            let
                ( newChildList, smallerDict ) =
                    List.foldr
                        (\child ( accChildList, accDict ) ->
                            let
                                childFromDict =
                                    Dict.get child.name accDict
                                        |> Maybe.withDefault child

                                dictWithoutGrandkids =
                                    moveChildrenToParent () childFromDict accDict

                                childWithGrandkids =
                                    Dict.get child.name dictWithoutGrandkids
                                        |> Maybe.withDefault childFromDict
                            in
                                ( childWithGrandkids :: accChildList
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
