module DAWG.ExpressionParser exposing (..)
import DAWG.Data exposing (..)
import Parser as P exposing (Parser, (|.), (|=), succeed, symbol, oneOf, lazy, spaces, end, getChompedString, chompIf, sequence, loop, Step(..))
import Char.Extra
import List.Extra as List

-- Top-level parser
expressionParser : Parser ExprAST
expressionParser =
  term |. spaces |. end

combine : (List ExprAST -> ExprAST) -> ExprAST -> List ExprAST -> ExprAST
combine f head tail =
  case tail of
    [] -> head
    _ -> f (head :: tail)

-- Term: Handles addition (+)
term : Parser ExprAST
term =
  succeed (combine A)
    |= factor
    |= loop [] (termHelper "+" factor)

-- Factor: Handles multiplication (.)
factor : Parser ExprAST
factor =
  succeed (combine M)
    |= primary
    |= loop [] (termHelper "." primary)

-- Helper for building lists of operations
termHelper : String -> Parser ExprAST -> List ExprAST -> Parser (Step (List ExprAST) (List ExprAST))
termHelper operator parser tail =
  oneOf
    [ succeed (\elem -> Loop (elem :: tail))
        |. symbol operator
        |. spaces
        |= parser
    , succeed (Done (List.reverse tail))
    ]

-- Primary: Variables or grouped terms
primary : Parser ExprAST
primary =
  oneOf
    [ variable
    , grouped
    ]

-- Single-character variable parser
variable : Parser ExprAST
variable =
  let
    cParser = \c -> not (Char.Extra.isSpace c || c == '+' || c == '.' || c == '(' || c == ')' || c == '!')
    parseChar finality =
      chompIf cParser
      |> getChompedString
      |> P.andThen
        (\s ->
          case String.uncons s of
            Just (c, "") -> succeed <| V (c, finality)
            _ -> P.problem "Variables must be single characters"
        )
  in
    oneOf
      [ symbol "!!" |> P.andThen (\_ -> succeed <| V ('!', 1))
      , symbol "!" |> P.andThen (\_ -> parseChar 1)
      , parseChar 0
      ]

-- Grouped expressions (parentheses)
grouped : Parser ExprAST
grouped =
    succeed identity
        |. symbol "("
        |. spaces
        |= lazy (\_ -> term)
        |. spaces
        |. symbol ")"

parseAlgebra : String -> Result (List P.DeadEnd) ExprAST
parseAlgebra =
  P.run expressionParser

stringToExpression : String -> ExprAST
stringToExpression s =
  case String.toList s of
    [] -> A []
    [x] -> V (x, 1)
    _ -> wordToTransitions s |> List.map V |> M

wordsToAlgebra : List String -> ExprAST
wordsToAlgebra xs =
  case xs of
    [] -> A []
    [x] -> stringToExpression x
    _ -> A <| List.map stringToExpression xs

wordToTransitions : String -> List Transition
wordToTransitions txt =
  Maybe.map
    (\(last, rest) ->
      List.map (\ch -> (ch, 0)) rest
      |> \transitions -> transitions ++ [(last, 1)]
    )
    (txt |> String.toList |> List.unconsLast)
  |> Maybe.withDefault [] -- don't accept an empty-string as valid.
