module DAWG_tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import DAWG as D
import Fuzz exposing (stringOfLengthBetween)

nodesAndWords : D.DAWG -> (Int, List String)
nodesAndWords d =
  (D.numNodes d, D.recognizedWords d)

suite : Test
suite =
  describe "The DAWG module"
    [ describe "an empty DAWG" -- Nest as many descriptions as you like.
      [ test "has no words in it" <|
        \_ ->
          nodesAndWords D.empty
          |> Expect.equal (1, [])
      ]
    , describe "adding a new transition"
      -- Expect.equal is designed to be used in pipeline style, like this.
      [ test "to an empty graph gives us one word" <|
        \_ ->
          D.empty
          |> D.addNewEdge 'a' True 0
          |> Tuple.first
          |> nodesAndWords
          |> Expect.equal (2, ["a"])

      , test "to a graph with one vertex gives sequential transitions" <|
        \_ ->
          D.empty
          |> D.addNewEdge 'a' False 0
          |> Tuple.first
          |> D.addNewEdge 'b' True 1
          |> Tuple.first
          |> nodesAndWords
          |> Expect.equal (3, ["ab"])

      , test "as an alternate gives two edges between two vertices" <|
        \_ ->
          D.empty
          |> D.addNewEdge 'a' True 0
          |> Tuple.first
          |> D.addNewEdge 'b' True 0
          |> Tuple.first |> D.debugDAWG "check"
          |> nodesAndWords
          |> Expect.equal (2, ["a", "b"])

      , test "to a graph with one vertex gives sequential transitions and another word" <|
        \_ ->
          D.empty
          |> D.addNewEdge 'a' True 0
          |> Tuple.first
          |> D.addNewEdge 'b' True 1
          |> Tuple.first
          |> nodesAndWords
          |> Expect.equal (3, ["a", "ab"])
      ]
    , describe "adding a new word"
      [ test "that is empty does nothing" <|
        \_ ->
          D.empty
          |> D.addString ""
          |> nodesAndWords
          |> Expect.equal (1, [])

      , test "with a single letter works" <|
        \_ ->
          D.empty
          |> D.addString "😃"
          |> nodesAndWords
          |> Expect.equal (2, ["😃"])

      , test "with two single letters works" <|
        \_ ->
          D.empty
          |> D.addString "ab"
          |> nodesAndWords
          |> Expect.equal (3, ["ab"])

      , test "with a multiple letters works" <|
        \_ ->
          D.empty
          |> D.addString "ghafūr"
          |> nodesAndWords
          |> Expect.equal (7, ["ghafūr"])

      , test "on top of the same old word does nothing" <|
        \_ ->
          D.empty
          |> D.addString "kurremkarmerruk"
          |> D.addString "kurremkarmerruk" |> D.debugDAWG "check"
          |> nodesAndWords
          |> Expect.equal (16, ["kurremkarmerruk"])

      , fuzz (stringOfLengthBetween 1 65) "that is randomly generated works" <|
        \randomlyGeneratedString ->
          D.empty
          |> D.addString randomlyGeneratedString
          |> nodesAndWords
          |> Expect.equal (String.length randomlyGeneratedString + 1, [randomlyGeneratedString])
      ]
    , describe "is stress-tested with"
      [ test "what-phat" <|
        \_ ->
          D.empty
          |> D.addString "what"
          |> D.addString "phat"
          |> nodesAndWords
          |> Expect.equal (5, ["phat", "what"])
      , test "what's up-whotsup-wassup-whatsapp-wazzup" <|
        \_ ->
          D.empty
          |> D.addString "what's up"
          |> D.addString "whotsup"
          |> D.addString "wassup"
          |> D.addString "whatsapp"
          |> D.addString "wazzup"
          |> nodesAndWords
          |> Expect.equal (21, ["wassup", "wazzup", "what's up", "whatsapp", "whotsup"])
      , test "able-unable-disable" <|
        \_ ->
          D.empty
          |> D.addString "able"
          |> D.addString "unable"
          |> D.addString "disable"
          |> nodesAndWords
          |> Expect.equal (9, ["able", "disable", "unable"])
      , test "tap-tar-top" <|
        \_ ->
          D.empty
          |> D.addString "tap"
          |> D.addString "tar"
          |> D.addString "top"
          |> nodesAndWords
          |> Expect.equal (5, ["tap", "tar", "top"])
      , test "nation-action" <|
        \_ ->
          D.empty
          |> D.addString "nation"
          |> D.addString "action"
          |> nodesAndWords
          |> Expect.equal (9, ["action", "nation"])
      , test "nation-action-function" <|
        \_ ->
          D.empty
          |> D.addString "nation"
          |> D.addString "action"
          |> D.addString "function"
          |> nodesAndWords
          |> Expect.equal (12, ["action", "function", "nation"])
      -- , test "nation-action-function-functionary" <|
      --   \_ ->
      --     D.empty
      --     |> D.addString "nation"
      --     |> D.addString "action"
      --     |> D.addString "function"
      --     |> D.addString "functionary"
      --     |> nodesAndWords
      --     |> Expect.equal ["action", "function", "functionary", "nation"]
      -- , test "nation-action-function-functionary-native" <|
      --   \_ ->
      --     D.empty
      --     |> D.addString "nation"
      --     |> D.addString "action"
      --     |> D.addString "function"
      --     |> D.addString "functionary"
      --     |> D.addString "native"
      --     |> nodesAndWords
      --     |> Expect.equal ["action", "function", "functionary", "nation", "native"]
      -- , test "nation-function-functionary" <|
      --   \_ ->
      --     D.empty
      --     |> D.addString "nation"
      --     |> D.addString "function"
      --     |> D.addString "functionary"
      --     |> nodesAndWords
      --     |> Expect.equal ["function", "functionary", "nation"]
      , test "fred-freddy" <|
        \_ ->
          D.empty
          |> D.addString "fred"
          |> D.addString "freddy"
          |> nodesAndWords
          |> Expect.equal (7, ["fred", "freddy"])
      , test "fred-freddy-frederick" <|
        \_ ->
          D.empty
          |> D.addString "fred"
          |> D.addString "freddy"
          |> D.addString "frederick" |> D.debugDAWG "check"
          |> nodesAndWords
          |> Expect.equal (12, ["fred", "freddy", "frederick"])
      , test "nation-action-nativity-activity" <|
        \_ ->
          D.empty
          |> D.addString "nation"
          |> D.addString "action"
          |> D.addString "nativity"
          |> D.addString "activity"
          |> nodesAndWords
          |> Expect.equal (13, ["action", "activity", "nation", "nativity"])
      -- , test "nation-action-nativity-activity-act" <|
      --   \_ ->
      --     D.empty
      --     |> D.addString "nation"
      --     |> D.addString "action"
      --     |> D.addString "nativity"
      --     |> D.addString "activity"
      --     |> D.addString "act" |> D.debugDAWG "check"
      --     |> nodesAndWords
      --     |> Expect.equal ["act", "action", "activity", "nation", "nativity"]
      , test "x-y" <|
        \_ ->
          D.empty
          |> D.addString "x"
          |> D.addString "y"
          |> nodesAndWords
          |> Expect.equal (2, ["x", "y"])
      , test "tark-tavk" <|
        \_ ->
          D.empty
          |> D.addString "tark"
          |> D.addString "tavk"
          |> nodesAndWords
          |> Expect.equal (5, ["tark", "tavk"])
      , test "tark-turkey" <|
        \_ ->
          D.empty
          |> D.addString "tark"
          |> D.addString "turkey"
          |> nodesAndWords
          |> Expect.equal (10, ["tark", "turkey"])
      , test "tark-shark" <|
        \_ ->
          D.empty
          |> D.addString "tark"
          |> D.addString "shark"
          |> nodesAndWords
          |> Expect.equal (6, ["shark", "tark"])
      , test "tar-tap" <|
        \_ ->
          D.empty
          |> D.addString "tar"
          |> D.addString "tap"
          |> nodesAndWords
          |> Expect.equal (4, ["tap", "tar"])
      ]
      , test "task-tark" <|
        \_ ->
          D.empty
          |> D.addString "task"
          |> D.addString "tark"
          |> nodesAndWords
          |> Expect.equal (5, ["tark", "task"])
      , test "task-tark-tork" <|
        \_ ->
          D.empty
          |> D.addString "task"
          |> D.addString "tark"
          |> D.addString "tork"
          |> nodesAndWords
          |> Expect.equal (7, ["tark", "task", "tork"])
      , test "tark-tork" <|
        \_ ->
          D.empty
          |> D.addString "tark"
          |> D.addString "tork"
          |> nodesAndWords
          |> Expect.equal (5, ["tark", "tork"])
      , test "task-hork-terk" <|
        \_ ->
          D.empty
          |> D.addString "task"
          |> D.addString "hork"
          |> D.addString "terk"
          |> nodesAndWords
          |> Expect.equal (7, ["hork", "task", "terk"])
    ]