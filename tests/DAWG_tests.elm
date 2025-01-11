module DAWG_tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import DAWG as D
import Fuzz exposing (stringOfLengthBetween)

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
          |> Tuple.first
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

      , test "to a graph with one vertex gives sequential transitions and another word" <|
        \_ ->
          D.empty
          |> D.addNewEdge 'a' True 0
          |> Tuple.first
          |> D.addNewEdge 'b' True 1
          |> Tuple.first
          |> D.recognizedWords
          |> Expect.equal ["a", "ab"]
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

      , fuzz (stringOfLengthBetween 1 65) "that is randomly generated works" <|
        \randomlyGeneratedString ->
          D.empty
          |> D.addString randomlyGeneratedString
          |> D.recognizedWords
          |> Expect.equal [randomlyGeneratedString]
      ]
    , describe "is stress-tested with"
      [ test "what-phat" <|
        \_ ->
          D.empty
          |> D.addString "what"
          |> D.addString "phat"
          |> D.recognizedWords
          |> Expect.equal ["phat", "what"]
      , test "what's up-whotsup-wassup-whatsapp-wazzup" <|
        \_ ->
          D.empty
          |> D.addString "what's up"
          |> D.addString "whotsup"
          |> D.addString "wassup"
          |> D.addString "whatsapp"
          |> D.addString "wazzup"
          |> D.recognizedWords
          |> Expect.equal ["wassup", "wazzup", "what's up", "whatsapp", "whotsup"]
      , test "able-unable-disable" <|
        \_ ->
          D.empty
          |> D.addString "able"
          |> D.addString "unable"
          |> D.addString "disable"
          |> D.recognizedWords
          |> Expect.equal ["able", "disable", "unable"]
      , test "tap-tar-top" <|
        \_ ->
          D.empty
          |> D.addString "tap"
          |> D.addString "tar"
          |> D.addString "top"
          |> D.recognizedWords
          |> Expect.equal ["tap", "tar", "top"]
      , test "nation-action" <|
        \_ ->
          D.empty
          |> D.addString "nation"
          |> D.addString "action"
          |> D.recognizedWords
          |> Expect.equal ["action", "nation"]
      , test "nation-action-function" <|
        \_ ->
          D.empty
          |> D.addString "nation"
          |> D.addString "action"
          |> D.addString "function"
          |> D.recognizedWords
          |> Expect.equal ["action", "function", "nation"]
      , test "nation-action-function-functionary" <|
        \_ ->
          D.empty
          |> D.addString "nation"
          |> D.addString "action"
          |> D.addString "function"
          |> D.addString "functionary"
          |> D.recognizedWords
          |> Expect.equal ["action", "function", "functionary", "nation"]
      , test "nation-action-function-functionary-native" <|
        \_ ->
          D.empty
          |> D.addString "nation"
          |> D.addString "action"
          |> D.addString "function"
          |> D.addString "functionary"
          |> D.addString "native"
          |> D.recognizedWords
          |> Expect.equal ["action", "function", "functionary", "nation", "native"]
      , test "nation-function-functionary" <|
        \_ ->
          D.empty
          |> D.addString "nation"
          |> D.addString "function"
          |> D.addString "functionary"
          |> D.recognizedWords
          |> Expect.equal ["function", "functionary", "nation"]
      , test "fred-freddy" <|
        \_ ->
          D.empty
          |> D.addString "fred"
          |> D.addString "freddy"
          |> D.recognizedWords
          |> Expect.equal ["fred", "freddy"]
      , test "fred-freddy-frederick" <|
        \_ ->
          D.empty
          |> D.addString "fred"
          |> D.addString "freddy"
          |> D.addString "frederick" |> D.debugDAWG "check"
          |> D.recognizedWords
          |> Expect.equal ["fred", "freddy", "frederick"]
      , test "nation-action-nativity-activity" <|
        \_ ->
          D.empty
          |> D.addString "nation"
          |> D.addString "action"
          |> D.addString "nativity"
          |> D.addString "activity"
          |> D.recognizedWords
          |> Expect.equal ["action", "activity", "nation", "nativity"]
      , test "nation-action-nativity-activity-act" <|
        \_ ->
          D.empty
          |> D.addString "nation"
          |> D.addString "action"
          |> D.addString "nativity"
          |> D.addString "activity"
          |> D.addString "act" |> D.debugDAWG "check"
          |> D.recognizedWords
          |> Expect.equal ["act", "action", "activity", "nation", "nativity"]
      , test "x-y" <|
        \_ ->
          D.empty
          |> D.addString "x"
          |> D.addString "y"
          |> D.recognizedWords
          |> Expect.equal ["x", "y"]
      , test "tark-tavk" <|
        \_ ->
          D.empty
          |> D.addString "tark"
          |> D.addString "tavk"
          |> D.recognizedWords
          |> Expect.equal ["tark", "tavk"]
      , test "tark-turkey" <|
        \_ ->
          D.empty
          |> D.addString "tark"
          |> D.addString "turkey"
          |> D.recognizedWords
          |> Expect.equal ["tark", "turkey"]
      , test "tark-shark" <|
        \_ ->
          D.empty
          |> D.addString "tark"
          |> D.addString "shark"
          |> D.recognizedWords
          |> Expect.equal ["shark", "tark"]
      ]
    ]