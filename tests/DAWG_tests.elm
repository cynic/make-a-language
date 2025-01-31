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
    , describe "algorithms can handle"
      [ test "two totally separate words" <|
        \_ ->
          D.fromWords ["abc", "def"]
          |> nodesAndWords
          |> Expect.equal (6, ["abc", "def"])
      ]
    , describe "handles prefix construction correctly when given"
      [ test "px-pxa-pya-pya" <|
        \_ ->
          D.fromWords ["px", "pxa", "pya", "pya"]
          |> nodesAndWords
          |> Expect.equal (4, ["px", "pxa", "pya"])
      , test "pxa-py-qya" <|
        \_ ->
          D.fromWords ["pxa", "py", "qya"]
          |> nodesAndWords
          |> Expect.equal (5, ["pxa", "py", "qya"])
      , test "pxa-py-pya-pya" <|
        \_ ->
          D.fromWords ["pxa", "py", "pya", "pya"]
          |> nodesAndWords
          |> Expect.equal (4, ["pxa", "py", "pya"])
      , test "py-pya-pya" <|
        \_ ->
          D.fromWords ["py", "pya"]
          |> nodesAndWords
          |> Expect.equal (4, ["py", "pya"])
      , test "py-pya" <|
        \_ ->
          D.fromWords ["py", "pya"]
          |> nodesAndWords
          |> Expect.equal (4, ["py", "pya"])
      , test "pya-py" <|
        \_ ->
          D.fromWords ["pya", "py"]
          |> nodesAndWords
          |> Expect.equal (4, ["py", "pya"])
      , test "px-pxa-pya-py" <|
        \_ ->
          D.fromWords ["px", "pxa", "pya", "py"] |> D.debugDAWG "check"
          |> nodesAndWords
          |> Expect.equal (4, ["px", "pxa", "py", "pya"])
      , test "pya-py-py" <|
        \_ ->
          D.fromWords ["pya", "py", "py"]
          |> nodesAndWords
          |> Expect.equal (4, ["py", "pya"])
      , test "py-py" <|
        \_ ->
          D.fromWords ["py", "py"]
          |> nodesAndWords
          |> Expect.equal (3, ["py"])
      , test "pxxa-pyy-pyya" <|
        \_ ->
          D.fromWords ["pxxa", "pyy", "pyya"]
          |> nodesAndWords
          |> Expect.equal (6, ["pxxa", "pyy", "pyya"])
      , test "pxxa-pyya-py" <|
        \_ ->
          D.fromWords ["pxxa", "pyya", "py"]
          |> nodesAndWords
          |> Expect.equal (6, ["pxxa", "py", "pyya"])
      , test "pxaab-pya-pyaab" <|
        \_ ->
          D.fromWords ["pxaab", "pya", "pyaab"]
          |> nodesAndWords
          |> Expect.equal (7, ["pxaab", "pya", "pyaab"])
      , test "pxba-py-pyyba" <|
        \_ ->
          D.fromWords ["pxba", "py", "pyyba"]
          |> nodesAndWords
          |> Expect.equal (6, ["pxba", "py", "pyyba"])
      , test "pxba-py-pyba" <|
        \_ ->
          D.fromWords ["pxba", "py", "pyba"]
          |> nodesAndWords
          |> Expect.equal (5, ["pxba", "py", "pyba"])
      , test "pxa-py-pya-pr-pra" <|
        \_ ->
          D.fromWords ["pxa", "py", "pya", "pr", "pra"]
          |> nodesAndWords
          |> Expect.equal (4, ["pr", "pra", "pxa", "py", "pya"])
      , test "s-ps" <|
        \_ ->
          D.fromWords ["s", "ps"]
          |> nodesAndWords
          |> Expect.equal (3, ["ps", "s"])
      , test "ps-s" <|
        \_ ->
          D.fromWords ["ps", "s"]
          |> nodesAndWords
          |> Expect.equal (3, ["ps", "s"])
      , test "aps-as" <|
        \_ ->
          D.fromWords ["aps", "as"]
          |> nodesAndWords
          |> Expect.equal (4, ["aps", "as"])
      , test "as-aps" <|
        \_ ->
          D.fromWords ["as", "aps"]
          |> nodesAndWords
          |> Expect.equal (4, ["aps", "as"])
      , test "aps-ps" <|
        \_ ->
          D.fromWords ["aps", "ps"]
          |> nodesAndWords
          |> Expect.equal (4, ["aps", "ps"])
      , test "apqzs-as" <|
        \_ ->
          D.fromWords ["apqzs", "as"]
          |> nodesAndWords
          |> Expect.equal (6, ["apqzs", "as"])
      , test "apqzs-azs" <|
        \_ ->
          D.fromWords ["apqzs", "azs"]
          |> nodesAndWords
          |> Expect.equal (6, ["apqzs", "azs"])
      , test "ate-create" <|
        \_ ->
          D.fromWords ["ate", "create"]
          |> nodesAndWords
          |> Expect.equal (7, ["ate", "create"])
      , test "create-ate" <|
        \_ ->
          D.fromWords ["create", "ate"]
          |> nodesAndWords
          |> Expect.equal (7, ["ate", "create"])
      , test "bars-ballad-bxrs" <|
        \_ ->
          D.fromWords ["bars", "ballad", "bxrs"]
          |> nodesAndWords
          |> Expect.equal (9, ["ballad", "bars", "bxrs"])
      , test "bats-bars-bxrs" <|
        \_ ->
          D.fromWords ["bats", "bars", "bxrs"]
          |> nodesAndWords
          |> Expect.equal (6, ["bars", "bats", "bxrs"])
      ]
    , describe "adding a new transition"
      -- Expect.equal is designed to be used in pipeline style, like this.
      [ test "to an empty graph gives us one word" <|
        \_ ->
          D.addString "a" D.empty
          |> nodesAndWords
          |> Expect.equal (2, ["a"])

      , test "to a graph with one vertex gives sequential transitions" <|
        \_ ->
          D.addString "ab" D.empty
          |> nodesAndWords
          |> Expect.equal (3, ["ab"])

      -- , test "as an alternate gives two edges between two vertices" <|
      --   \_ ->
      --     D.empty
      --     |> D.addNewEdge 'a' True [] 0
      --     |> Tuple.first
      --     |> D.addNewEdge 'b' True [] 0
      --     |> Tuple.first |> D.debugDAWG "check"
      --     |> nodesAndWords
      --     |> Expect.equal (2, ["a", "b"])

      -- , test "to a graph with two alternates gives three edges between two vertices" <|
      --   \_ ->
      --     D.empty
      --     |> D.addNewEdge 'a' True [] 0
      --     |> Tuple.first
      --     |> D.addNewEdge 'b' True [] 0
      --     |> Tuple.first
      --     |> D.addNewEdge 'c' True [] 0
      --     |> Tuple.first |> D.debugDAWG "check"
      --     |> nodesAndWords
      --     |> Expect.equal (2, ["a", "b", "c"])

      , test "to a graph with one vertex gives sequential transitions and another word" <|
        \_ ->
          D.addString "a" D.empty
          |> D.addString "ab"
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

      , fuzz (Fuzz.asciiStringOfLengthBetween 1 65) "that is randomly generated works" <|
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
          |> Expect.equal (16, ["wassup", "wazzup", "what's up", "whatsapp", "whotsup"])
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
          |> Expect.equal (8, ["action", "nation"])
      , test "nation-action-function" <|
        \_ ->
          D.empty
          |> D.addString "nation"
          |> D.addString "action"
          |> D.addString "function"
          |> nodesAndWords
          |> Expect.equal (10, ["action", "function", "nation"])
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
          |> Expect.equal (11, ["fred", "freddy", "frederick"])
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
          |> Expect.equal (9, ["tark", "turkey"])
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
          |> Expect.equal (6, ["tark", "task", "tork"])
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
      , test "phong-pring" <|
        \_ ->
          D.empty
          |> D.addString "phong"
          |> D.addString "pring"
          |> nodesAndWords
          |> Expect.equal (7, ["phong", "pring"])
      , test "phong-pring-pheng" <|
        \_ ->
          D.empty
          |> D.addString "phong"
          |> D.addString "pring"
          |> D.addString "pheng"
          |> nodesAndWords
          |> Expect.equal (7, ["pheng", "phong", "pring"])
      , test "prong-pring-pheng-prin" <|
        \_ ->
          D.empty
          |> D.addString "prong"
          |> D.addString "pring"
          |> D.addString "pheng"
          |> D.addString "prin"
          |> nodesAndWords
          |> Expect.equal (8, ["pheng", "prin", "pring", "prong"])
      , test "prong-pring-prin-phong" <|
        \_ ->
          D.empty
          |> D.addString "prong"
          |> D.addString "pring"
          |> D.addString "prin"
          |> D.addString "phong"
          |> nodesAndWords
          |> Expect.equal (8, ["phong", "prin", "pring", "prong"])
      , test "tar-tap-box" <|
        \_ ->
          D.empty
          |> D.addString "tar"
          |> D.addString "tap"
          |> D.addString "tax"
          |> nodesAndWords
          |> Expect.equal (4, ["tap", "tar", "tax"])
      , test "try-pry-cry" <|
        \_ ->
          D.empty
          |> D.addString "try"
          |> D.addString "pry"
          |> D.addString "cry"
          |> nodesAndWords
          |> Expect.equal (4, ["cry", "pry", "try"])
    ]