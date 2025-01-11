module DAWG_tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import DAWG as D

suite : Test
suite =
  describe "The DAWG module"
    [ describe "an empty DAWG" -- Nest as many descriptions as you like.
      [ test "has no words in it" <|
        \_ ->
          D.recognizedWords D.empty
          |> Expect.equal []
      ]
    , describe "adding a new transition"
      -- Expect.equal is designed to be used in pipeline style, like this.
      [ test "to an empty graph gives us one word" <|
        \_ ->
          D.empty
          |> D.addNewEdge 'a' True 0
          |> Tuple.first
          |> D.recognizedWords
          |> Expect.equal ["a"]

      , test "to a graph with one vertex gives sequential transitions" <|
        \_ ->
          D.empty
          |> D.addNewEdge 'a' False 0
          |> Tuple.first
          |> D.addNewEdge 'b' True 1
          |> Tuple.first |> D.debugDAWG "graph"
          |> D.recognizedWords
          |> Expect.equal ["ab"]

      , test "as an alternate gives two edges between two vertices" <|
        \_ ->
          D.empty
          |> D.addNewEdge 'a' True 0
          |> Tuple.first
          |> D.addNewEdge 'b' True 0
          |> Tuple.first
          |> D.recognizedWords
          |> Expect.equal ["a", "b"]
      ]
    , describe "adding a new word"
      [ test "that is empty does nothing" <|
        \_ ->
          D.empty
          |> D.addString ""
          |> D.recognizedWords
          |> Expect.equal []

      , test "with a single letter works" <|
        \_ ->
          D.empty
          |> D.addString "😃"
          |> D.recognizedWords
          |> Expect.equal ["😃"]

      , test "with two single letters works" <|
        \_ ->
          D.empty
          |> D.addString "ab"
          |> D.recognizedWords
          |> Expect.equal ["ab"]

      , test "with a multiple letters works" <|
        \_ ->
          D.empty
          |> D.addString "ghafūr"
          |> D.recognizedWords
          |> Expect.equal ["ghafūr"]

      -- , fuzz string "with multiple letters works" <|
      --   \randomlyGeneratedString ->
      --     D.empty
      --     |> D.addString randomlyGeneratedString
      --     |> D.recognizedWords
      --     |> Expect.equal [randomlyGeneratedString]
      ]
    ]