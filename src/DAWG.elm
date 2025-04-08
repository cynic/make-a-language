module DAWG exposing (..)
import DAWG.Data exposing (..)
import DAWG.ExpressionParser exposing (parseAlgebra, wordsToAlgebra)
import DAWG.Debugging exposing (..)
import Set
import IntDict
import DAWG.Simplify as Simplify
import DAWG.Simplify3 as Simplify3
import DAWG.Simplify4 as Simplify4

fromLines : String -> DAWG
fromLines s =
  s
  |> String.replace " " ""
  |> String.replace "\n" ""
  |> parseAlgebra
  |> Result.map Simplify.algebraToDAWG
  |> Result.withDefault empty

{-| True if at least one transition terminates at this node -}
isTerminalNode : Node -> Bool
isTerminalNode node =
  IntDict.foldl
    (\_ conn state ->
      state || 
        Set.foldl
          (\(_, isFinal) state_ -> state_ || isFinal == 1)
          False
          conn
    )
    False
    (node.incoming)

{--
  User-facing functions (and a few helpers thereof)
--}

fromWords : List String -> DAWG
fromWords =
  wordsToAlgebra >> Simplify.algebraToDAWG

fromWords3 : List String -> DAWG
fromWords3 =
  wordsToAlgebra >> Simplify3.algebraToDAWG

fromWords4 : List String -> DAWG
fromWords4 =
  Simplify4.toDAWG