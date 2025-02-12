module DAWG_tests exposing (..)

import Expect
import Fuzz
import Test exposing (..)
import DAWG as D
import Fuzz
import List.Extra

nodesAndWords : D.DAWG -> (Int, List String)
nodesAndWords d =
  (D.numNodes d, D.verifiedRecognizedWords d)

standardTestForWords : List String -> Int -> Int -> Expect.Expectation
standardTestForWords words expectedNodes expectedEdges =
  let
    expectedRecognized = List.sort words |> List.Extra.unique |> List.filter ((/=) "")
    czech : List (List String) -> Expect.Expectation
    czech l =
      case l of
        [] ->
          Expect.pass
        x::rest ->
          let
            dawg = D.fromWords x
            edges = D.numEdges dawg
            nodes = D.numNodes dawg
            recognized = D.verifiedRecognizedWords dawg
          in
            if recognized /= expectedRecognized then
              Debug.log "Failure on recognized words of permutation" x
              |> \_ -> Expect.equal recognized expectedRecognized
            else if nodes /= expectedNodes then
              Debug.log "Failure on node-count of permutation" x
              |> \_ -> Expect.equal nodes expectedNodes
            else if edges /= expectedEdges then
              Debug.log "Failure on edge-count of permutation" x
              |> \_ -> Expect.equal edges expectedEdges
            else
              czech rest
            
  in
    czech (List.Extra.permutations words)

suite : Test
suite =
  describe "The DAWG module"
    [ describe "an empty DAWG" -- Nest as many descriptions as you like.
      [ test "has no words in it" <|
        \_ ->
          (D.numNodes D.empty, D.recognizedWords D.empty)
          |> Expect.equal (1, [])
      ]
    , describe "algorithms can handle"
      [ test "two totally separate words" <|
        \_ ->
          standardTestForWords ["abc", "def"] 6 6
      ]
    , describe "handles prefix construction correctly when given"
      [ test "px-pxa-pya-pya" <|
        \_ ->
          standardTestForWords ["px", "pxa", "pya", "pya"] 4 3
      , test "pxa-py-qya" <|
        \_ ->
          standardTestForWords ["pxa", "py", "qya"] 5 6
      , test "pxa-py-pya-pya" <|
        \_ ->
          standardTestForWords ["pxa", "py", "pya", "pya"] 4 3
      , test "py-pya-pya" <|
        \_ ->
          standardTestForWords ["py", "pya", "pya"] 4 3
      , test "py-pya" <|
        \_ ->
          standardTestForWords ["py", "pya"] 4 3
      , test "px-pxa-pya-py" <|
        \_ ->
          standardTestForWords ["px", "pxa", "pya", "py"] 4 3
      , test "pya-py-py" <|
        \_ ->
          standardTestForWords ["pya", "py", "py"] 4 3
      , test "py-py" <|
        \_ ->
          standardTestForWords ["py", "py"] 3 2
      , test "pxxa-pyy-pyya" <|
        \_ ->
          standardTestForWords ["pxxa", "pyy", "pyya"] 6 6
      , test "pxxa-pyya-py" <|
        \_ ->
          standardTestForWords ["pxxa", "pyya", "py"] 6 6
      , test "pxaab-pya-pyaab" <|
        \_ ->
          standardTestForWords ["pxaab", "pya", "pyaab"] 7 7
      , test "pxba-py-pyyba" <|
        \_ ->
          standardTestForWords ["pxba", "py", "pyyba"] 6 6
      , test "pxba-py-pyba" <|
        \_ ->
          standardTestForWords ["pxba", "py", "pyba"] 5 4
      , test "pxa-py-pya-pr-pra" <|
        \_ ->
          standardTestForWords ["pxa", "py", "pya", "pr", "pra"] 4 3
      , test "ps-s" <|
        \_ ->
          standardTestForWords ["ps", "s"] 3 3
      , test "aps-as" <|
        \_ ->
          standardTestForWords ["aps", "as"] 4 4
      , test "aps-ps" <|
        \_ ->
          standardTestForWords ["aps", "ps"] 4 4
      , test "apqzs-as" <|
        \_ ->
          standardTestForWords ["apqzs", "as"] 6 6
      , test "apqzs-azs" <|
        \_ ->
          standardTestForWords ["apqzs", "azs"] 6 6
      , test "ate-create" <|
        \_ ->
          standardTestForWords ["ate", "create"] 7 7
      , test "bars-ballad-bxrs" <|
        \_ ->
          standardTestForWords ["bars", "ballad", "bxrs"] 9 10
      , test "bats-bars-bxrs" <|
        \_ ->
          standardTestForWords ["bats", "bars", "bxrs"] 6 6
      , test "naton-acton-natit-actit" <|
        \_ ->
          standardTestForWords ["naton", "acton", "natit", "actit"] 8 9
      , test "aton-cton-atit-ctit" <|
        \_ ->
          standardTestForWords ["aton", "cton", "atit", "ctit"] 6 6
      , test "ato-cto-ati" <|
        \_ ->
          standardTestForWords ["ato", "cto", "ati"] 6 6
      , test "ato-cto-at" <|
        \_ ->
          standardTestForWords ["ato", "cto", "at"] 5 5
      , test "ato-cto-ati-cti" <|
        \_ ->
          standardTestForWords ["ato", "cto", "ati", "cti"] 4 3
      , test "ato-cto" <|
        \_ ->
          standardTestForWords ["ato", "cto"] 4 3
      , test "pqt-zvt-pqr" <|
        \_ ->
          standardTestForWords ["pqt", "zvt", "pqr"] 6 6
      , test "pqt-zvt-zvr-pqr-pvt-zqr-pvr-zqt" <|
        \_ ->
          standardTestForWords ["pqt", "zvt", "zvr", "pqr", "pvt", "zqr", "pvr", "zqt"] 4 3
      , test "zvt-pqt-pqr" <|
        \_ ->
          D.fromWords ["zvt", "pqt", "pqr"]
          |> nodesAndWords
          |> Expect.equal (6, ["pqr", "pqt", "zvt"])
      , test "pqt-prt-pqr-zvt" <|
        \_ ->
          D.fromWords ["pqt", "prt", "pqr", "zvt"]
          |> nodesAndWords
          |> Expect.equal (6, ["pqr", "pqt", "prt", "zvt"])
      , test "pqt-prt-zvt-pqr" <|
        \_ ->
          D.fromWords ["pqt", "prt", "zvt", "pqr"]
          |> nodesAndWords
          |> Expect.equal (6, ["pqr", "pqt", "prt", "zvt"])
      , test "pqt-zvt-zvr-pqr" <|
        \_ ->
          D.fromWords ["pqt", "zvt", "zvr", "pqr"]
          |> nodesAndWords
          |> Expect.equal (5, ["pqr", "pqt", "zvr", "zvt"])
      , test "pqt-zvt-pqarcz-pqr" <|
        \_ ->
          D.fromWords ["pqt", "zvt", "pqarcz", "pqr"]
          |> nodesAndWords
          |> Expect.equal (9, ["pqarcz", "pqr", "pqt", "zvt"])
      , test "pqt-zvt-zvarc-pqr" <|
        \_ ->
          D.fromWords ["pqt", "zvt", "zvarc", "pqr"]
          |> nodesAndWords
          |> Expect.equal (8, ["pqr", "pqt", "zvarc", "zvt"])
      , test "pqt-zvt-pqrr" <|
        \_ ->
          D.fromWords ["pqt", "zvt", "pqrr"]
          |> nodesAndWords
          |> Expect.equal (7, ["pqrr", "pqt", "zvt"])
      , test "pqt-zvt-zvxt" <|
        \_ ->
          D.fromWords ["pqt", "zvt", "zvxt"]
          |> nodesAndWords
          |> Expect.equal (6, ["pqt", "zvt", "zvxt"])
      , test "x-y-xx" <|
        \_ ->
          D.fromWords ["x", "y", "xx"]
          |> nodesAndWords
          |> Expect.equal (3, ["x", "xx", "y"])
      , test "kxto-pzto-pzt" <|
        \_ ->
          D.fromWords ["kxto", "pzto", "pzt"]
          |> nodesAndWords
          |> Expect.equal (7, ["kxto", "pzt", "pzto"])
      , test "crv-ax-cx-arv" <|
        \_ ->
          D.fromWords ["crv", "ax", "cx", "arv"]
          |> nodesAndWords
          |> Expect.equal (4, ["arv", "ax", "crv", "cx"])
      , test "pato-qcto-pat" <|
        \_ ->
          D.fromWords ["pato", "qcto", "pat"]
          |> nodesAndWords
          |> Expect.equal (7, ["pat", "pato", "qcto"])
      , test "pato-qcto-pati" <|
        \_ ->
          D.fromWords ["pato", "qcto", "pati"]
          |> nodesAndWords
          |> Expect.equal (8, ["pati", "pato", "qcto"])
      , test "pato-peto-qcto-pati" <|
        \_ ->
          D.fromWords ["pato", "peto", "qcto", "pati"]
          |> nodesAndWords
          |> Expect.equal (8, ["pati", "pato", "peto", "qcto"])
      , test "ato-cto-atoz" <|
        \_ ->
          D.fromWords ["ato", "cto", "atoz"]
          |> nodesAndWords
          |> Expect.equal (7, ["ato", "atoz", "cto"])
      , test "pato-qcto-patoz" <|
        \_ ->
          D.fromWords ["pato", "qcto", "patoz"]
          |> nodesAndWords
          |> Expect.equal (9, ["pato", "patoz", "qcto"])
      , test "atpvnxb-ctpvnxb-atprxb-ctprxb-atprxy" <|
        \_ ->
          D.fromWords ["atpvnxb", "ctpvnxb", "atprxb", "ctprxb", "atprxy"]
          |> nodesAndWords
          |> Expect.equal (13, ["atprxb", "atprxy", "atpvnxb", "ctprxb", "ctpvnxb"])
      , test "xa-y-ya" <|
        \_ ->
          D.fromWords ["xa", "y", "ya"]
          |> nodesAndWords
          |> Expect.equal (3, ["xa", "y", "ya"])
      , test "xa-y-yaa" <|
        \_ ->
          D.fromWords ["xa", "y", "yaa"]
          |> nodesAndWords
          |> Expect.equal (4, ["xa", "y", "yaa"])
      , test "xa-y-yaaa" <|
        \_ ->
          D.fromWords ["xa", "y", "yaaa"]
          |> nodesAndWords
          |> Expect.equal (5, ["xa", "y", "yaaa"])
      , test "xa-y-yaaaa" <|
        \_ ->
          D.fromWords ["xa", "y", "yaaaa"]
          |> nodesAndWords
          |> Expect.equal (6, ["xa", "y", "yaaaa"])
      , test "av-rv-kv-kva" <|
        \_ ->
          D.fromWords ["av", "rv", "kv", "kva"]
          |> nodesAndWords
          |> Expect.equal (5, ["av", "kv", "kva", "rv"])
      , test "rxa-ry-pva-py-pxa" <|
        \_ ->
          D.fromWords ["rxa", "ry", "pva", "py", "pxa"]
          |> nodesAndWords
          |> Expect.equal (5, ["pva", "pxa", "py", "rxa", "ry"])
      , test "xav-pbt-pzv" <|
        \_ ->
          D.fromWords ["xav", "pbt", "pzv"]
          |> nodesAndWords
          |> Expect.equal (6, ["pbt", "pzv", "xav"])
      , test "park-qv-qsk" <|
        \_ ->
          D.fromWords ["park", "qv", "qsk"]
          |> nodesAndWords
          |> Expect.equal (6, ["park", "qsk", "qv"])
      , test "park-qv-qvrk" <|
        \_ ->
          D.fromWords ["park", "qv", "qvrk"]
          |> nodesAndWords
          |> Expect.equal (6, ["park", "qv", "qvrk"])
      , test "an-tn-x-tx" <|
        \_ ->
          D.fromWords ["an", "tn", "x", "tx"]
          |> nodesAndWords
          |> Expect.equal (4, ["an", "tn", "tx", "x"])
      , test "x-y-am-xm" <|
        \_ ->
          D.fromWords ["x", "y", "am", "xm"]
          |> nodesAndWords
          |> Expect.equal (3, ["am", "x", "xm", "y"]) 
      , test "x-b-bc-ac-bx" <|
        \_ ->
          D.fromWords ["x", "b", "bc", "ac", "bx"]
          |> nodesAndWords
          |> Expect.equal (4, ["ac", "b", "bc", "bx", "x"])
      , test "x-b-bc-ac-bz" <|
        \_ ->
          D.fromWords ["x", "b", "bc", "ac", "bz"]
          |> nodesAndWords
          |> Expect.equal (4, ["ac", "b", "bc", "bz", "x"])
      , test "px-pb-pbc-pac-pbz" <|
        \_ ->
          D.fromWords ["px", "pb", "pbc", "pac", "pbz"]
          |> nodesAndWords
          |> Expect.equal (5, ["pac", "pb", "pbc", "pbz", "px"])
      , test "x-b-bc-ac-bzo" <|
        \_ ->
          D.fromWords ["x", "b", "bc", "ac", "bzo"]
          |> nodesAndWords
          |> Expect.equal (5, ["ac", "b", "bc", "bzo", "x"])
      , test "kp-gx-ax-gp" <|
        \_ ->
          D.fromWords ["kp","gx","ax","gp"]
          |> nodesAndWords
          |> Expect.equal (5, ["ax", "gp", "gx", "kp"])
      , test "towxm-tbwxm-tovxm-tbvxm-towym-tbwym-tovym-tbvym" <|
        \_ ->
          D.fromWords ["towxm", "tbwxm", "tovxm", "tbvxm", "towym", "tbwym", "tovym", "tbvym"]
          |> nodesAndWords
          |> Expect.equal (6, ["tbvxm","tbvym","tbwxm","tbwym","tovxm","tovym","towxm","towym"])
      , test "owx-bwx-ovx-bvx-owy" <|
        \_ ->
          D.fromWords ["owx","bwx","ovx","bvx","owy"]
          |> nodesAndWords
          |> Expect.equal (6, ["bvx","bwx","ovx","owx","owy"])
      , test "owx-bwx-ovx-bvx-bv" <|
        \_ ->
          D.fromWords ["owx","bwx","ovx","bvx","bv"]
          |> nodesAndWords
          |> Expect.equal (5, ["bv", "bvx","bwx","ovx","owx"])
      , test "be-dv-cv-a-de" <|
        \_ ->
          D.fromWords ["be","dv","cv","a","de"]
          |> nodesAndWords
          |> Expect.equal (5, ["a","be","cv","de","dv"])
      , test "ttal-ntl-ntal" <|
        \_ ->
          D.fromWords ["ttal","ntl","ntal"]
          |> nodesAndWords
          |> Expect.equal (7, ["ntal","ntl","ttal"])
      , test "ttal-tyal-ntl-ntal" <|
        \_ ->
          D.fromWords ["ttal","tyal","ntl","ntal"]
          |> nodesAndWords
          |> Expect.equal (7, ["ntal","ntl","ttal","tyal"])
      , test "tt-ttal-nt-ntl-ntal" <|
        \_ ->
          D.fromWords ["tt","ttal","nt","ntl","ntal"]
          |> nodesAndWords
          |> Expect.equal (7, ["nt","ntal","ntl","tt","ttal"])
      , test "tstabl-nstabl-nstl" <|
        \_ ->
          D.fromWords ["tstabl","nstabl","nstl"]
          |> nodesAndWords
          |> Expect.equal (10, ["nstabl","nstl","tstabl"])
      , test "tsbl-nsbl-nsl" <|
        \_ ->
          D.fromWords ["tsbl","nsbl","nsl"]
          |> nodesAndWords
          |> Expect.equal (7, ["nsbl","nsl","tsbl"])
      , test "nst-nsl-tst-tsl-nstl" <|
        \_ ->
          D.fromWords ["nst", "nsl", "tst", "tsl", "nstl"]
          |> nodesAndWords
          |> Expect.equal (7, ["nsl", "nst", "nstl", "tsl", "tst"])
      , test "tst-tstabl-nst-nstabl-nstl" <|
        \_ ->
          D.fromWords ["tst","tstabl","nst","nstabl","nstl"]
          |> nodesAndWords
          |> Expect.equal (10, ["nst","nstabl","nstl","tst","tstabl"])
      , test "test-testable-tester-nest-nestable-nester-nestle" <|
        \_ ->
          D.fromWords ["test","testable","tester","nest","nestable","nester","nestle"]
          |> nodesAndWords
          |> Expect.equal (14, ["nest","nestable","nester","nestle","test","testable","tester"])
      , test "test-testable-tester-nest-nestable-nester-ne" <|
        \_ ->
          D.fromWords ["test","testable","tester","nest","nestable","nester","ne"]
          |> nodesAndWords
          |> Expect.equal (11, ["ne","nest","nestable","nester","test","testable","tester"])
      , test "tere-cere-te" <|
        \_ ->
          D.fromWords ["tere","cere","te"]
          |> nodesAndWords
          |> Expect.equal (6, ["cere","te","tere"])
      , test "teve-ceve-cyve-tyve-te" <|
        \_ ->
          D.fromWords ["teve", "ceve", "cyve", "tyve", "te"]
          |> nodesAndWords
          |> Expect.equal (6, ["ceve","cyve", "te", "teve", "tyve"])
      , test "ayxpayx-byxpayx-ayxpbyx-byxpbyx-ayx" <|
        \_ ->
          D.fromWords ["ayxpayx", "byxpayx", "ayxpbyx", "byxpbyx", "ayx"]
          |> nodesAndWords
          |> Expect.equal (10, ["ayx", "ayxpayx", "ayxpbyx", "byxpayx", "byxpbyx"])
      , test "ayxpayx-byxpayx-ayxpbyx-byxpbyx-ayxpayx" <|
        \_ ->
          D.fromWords ["ayxpayx", "byxpayx", "ayxpbyx", "byxpbyx", "ayxpayx"]
          |> nodesAndWords
          |> Expect.equal (8, ["ayxpayx", "ayxpbyx", "byxpayx", "byxpbyx"])
      , test "ayayx-byayx-aybyx" <|
        \_ ->
          D.fromWords ["ayayx", "byayx", "aybyx"]
          |> nodesAndWords
          |> Expect.equal (8, ["ayayx", "aybyx", "byayx"])
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
          |> \d -> (D.numNodes d, D.recognizedWords d) -- note: not `verifiedRecognizedWords`
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

    , fuzz (Fuzz.listOfLengthBetween 2 8 (Fuzz.asciiStringOfLengthBetween 1 5)) "always recognizes exactly the unique short words that it is given" <|
      \listOfStrings ->
        D.fromWords (List.Extra.unique listOfStrings)
        |> D.verifiedRecognizedWords
        |> Expect.equal (List.sort <| List.Extra.unique listOfStrings)

    , fuzz (Fuzz.listOfLengthBetween 3 7 (Fuzz.asciiStringOfLengthBetween 5 8)) "always recognizes exactly the unique long words that it is given" <|
      \listOfStrings ->
        D.fromWords (List.Extra.unique listOfStrings)
        |> D.verifiedRecognizedWords
        |> Expect.equal (List.sort <| List.Extra.unique listOfStrings)

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
          |> Expect.equal (11, ["action", "activity", "nation", "nativity"])
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