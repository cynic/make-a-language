module DAWG_tests exposing (..)

import Expect
import Fuzz
import Test exposing (..)
import DAWG as D
import List.Extra as List

nodesAndWords : D.DAWG -> (Int, List String)
nodesAndWords d =
  (D.numNodes d, D.verifiedRecognizedWords d)


czech : List (List String) -> List String -> Int -> Int -> Expect.Expectation
czech l expectedRecognized expectedNodes expectedEdges =
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
          czech rest expectedRecognized expectedNodes expectedEdges


expectedWords : List String -> List String
expectedWords = List.sort >> List.unique >> List.filter ((/=) "")

standardTestForWords : List String -> Int -> Int -> Expect.Expectation
standardTestForWords words =
  czech (List.permutations words) (expectedWords words)

cheapTestForWords : List String -> Int -> Int -> Expect.Expectation
cheapTestForWords words =
  czech [words] (expectedWords words)

suite : Test
suite =
  describe "The DAWG module"
    [ -- Nest as many descriptions as you like.
    describe "handles construction correctly when given"
      [
        test "px-pxa-pya-pya" <|
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
      , test "pqt-zqr-pvr-pvt" <|
        \_ ->
          standardTestForWords ["pqt", "zqr", "pvr", "pvt"] 7 8
      , test "pqt-prt-pqr-zvt" <|
        \_ ->
          standardTestForWords ["pqt", "prt", "pqr", "zvt"] 6 7
      , test "pqt-zvt-zvr-pqr" <|
        \_ ->
          standardTestForWords ["pqt", "zvt", "zvr", "pqr"] 5 5
      , test "pqt-zvt-pqarcz-pqr" <|
        \_ ->
          standardTestForWords ["pqt", "zvt", "pqarcz", "pqr"] 9 10
      , test "pqt-zvt-zvarc-pqr" <|
        \_ ->
          standardTestForWords ["pqt", "zvt", "zvarc", "pqr"] 8 9
      , test "pqt-zvt-pqrr" <|
        \_ ->
          standardTestForWords ["pqt", "zvt", "pqrr"] 7 8
      , test "pqt-zvt-zvxt" <|
        \_ ->
          standardTestForWords ["pqt", "zvt", "zvxt"] 6 7
      , test "x-y-xx" <|
        \_ ->
          standardTestForWords ["x", "y", "xx"] 3 3
      , test "kxto-pzto-pzt" <|
        \_ ->
          standardTestForWords ["kxto", "pzto", "pzt"] 7 7
      , test "crv-ax-cx-arv" <|
        \_ ->
          standardTestForWords ["crv", "ax", "cx", "arv"] 4 4
      , test "pato-qcto-pat" <|
        \_ ->
          standardTestForWords ["pato", "qcto", "pat"] 7 7
      , test "pato-qcto-pati" <|
        \_ ->
          standardTestForWords ["pato", "qcto", "pati"] 8 8
      , test "pato-peto-qcto-pati" <|
        \_ ->
          standardTestForWords ["pato", "peto", "qcto", "pati"] 8 9
      , test "ato-cto-atoz" <|
        \_ ->
          standardTestForWords ["ato", "cto", "atoz"] 7 7
      , test "pato-qcto-patoz" <|
        \_ ->
          standardTestForWords ["pato", "qcto", "patoz"] 9 9
      , test "atpvnxb-ctpvnxb-atprxb-ctprxb-atprxy" <|
        \_ ->
          standardTestForWords ["atpvnxb", "ctpvnxb", "atprxb", "ctprxb", "atprxy"] 13 15
      , test "xa-y-ya" <|
        \_ ->
          standardTestForWords ["xa", "y", "ya"] 3 2
      , test "xa-y-yaa" <|
        \_ ->
          standardTestForWords ["xa", "y", "yaa"] 4 4
      , test "xa-y-yaaa" <|
        \_ ->
          standardTestForWords ["xa", "y", "yaaa"] 5 5
      , test "xa-y-yaaaa" <|
        \_ ->
          standardTestForWords ["xa", "y", "yaaaa"] 6 6
      , test "av-rv-kv-kva" <|
        \_ ->
          standardTestForWords ["av", "rv", "kv", "kva"] 5 5
      , test "rxa-ry-pva-py-pxa" <|
        \_ ->
          standardTestForWords ["rxa", "ry", "pva", "py", "pxa"] 5 7
      , test "xav-pbt-pzv" <|
        \_ ->
          standardTestForWords ["xav", "pbt", "pzv"] 6 7
      , test "park-qv-qsk" <|
        \_ ->
          standardTestForWords ["park", "qv", "qsk"] 6 7
      , test "park-qv-qvrk" <|
        \_ ->
          standardTestForWords ["park", "qv", "qvrk"] 6 6
      , test "an-tn-x-tx" <|
        \_ ->
          standardTestForWords ["an", "tn", "x", "tx"] 4 5
      , test "x-y-am-xm" <|
        \_ ->
          standardTestForWords ["x", "y", "am", "xm"] 3 3
      , test "x-b-bc-ac-bx" <|
        \_ ->
          standardTestForWords ["x", "b", "bc", "ac", "bx"] 4 5
      , test "px-pb-pbc-pac-pbz" <|
        \_ ->
          standardTestForWords ["px", "pb", "pbc", "pac", "pbz"] 5 6
      , test "x-b-bc-ac-bzo" <|
        \_ ->
          standardTestForWords ["x", "b", "bc", "ac", "bzo"] 5 7
      , test "kp-gx-ax-gp" <|
        \_ ->
          standardTestForWords ["kp","gx","ax","gp"] 5 6
      , test "owx-bwx-ovx-bvx-owy" <|
        \_ ->
          standardTestForWords ["owx","bwx","ovx","bvx","owy"] 6 7
      , test "owx-bwx-ovx-bvx-bv" <|
        \_ ->
          standardTestForWords ["owx","bwx","ovx","bvx","bv"] 5 5
      , test "be-dv-cv-a-de" <|
        \_ ->
          standardTestForWords ["be","dv","cv","a","de"] 5 7
      , test "ttal-ntl-ntal" <|
        \_ ->
          standardTestForWords ["ttal","ntl","ntal"] 7 8
      , test "ttal-tyal-ntl-ntal" <|
        \_ ->
          standardTestForWords ["ttal","tyal","ntl","ntal"] 7 8
      , test "tt-ttal-nt-ntl-ntal" <|
        \_ ->
          standardTestForWords ["tt","ttal","nt","ntl","ntal"] 7 8
      , test "tstabl-nstabl-nstl" <|
        \_ ->
          standardTestForWords ["tstabl","nstabl","nstl"] 10 11
      , test "tsbl-nsbl-nsl" <|
        \_ ->
          standardTestForWords ["tsbl","nsbl","nsl"] 7 8
      , test "nst-nsl-tst-tsl-nstl" <|
        \_ ->
          standardTestForWords ["nst", "nsl", "tst", "tsl", "nstl"] 7 8
      , test "tst-tstabl-nst-nstabl-nstl" <|
        \_ ->
          standardTestForWords ["tst","tstabl","nst","nstabl","nstl"] 10 11
      , test "teste-ne-neste" <|
        \_ ->
          standardTestForWords ["teste", "ne", "neste"] 7 7
      , test "tere-cere-te" <|
        \_ ->
          standardTestForWords ["tere","cere","te"] 6 6
      , test "teve-ceve-cyve-tyve-te" <|
        \_ ->
          standardTestForWords ["teve", "ceve", "cyve", "tyve", "te"] 6 6
      , test "ayxpayx-byxpayx-ayxpbyx-byxpbyx-ayx" <|
        \_ ->
          standardTestForWords ["ayxpayx", "byxpayx", "ayxpbyx", "byxpbyx", "ayx"] 10 10
      , test "ayxpayx-byxpayx-ayxpbyx-byxpbyx-ayxpayx" <|
        \_ ->
          standardTestForWords ["ayxpayx", "byxpayx", "ayxpbyx", "byxpbyx", "ayxpayx"] 8 7
      , test "ayayx-byayx-aybyx" <|
        \_ ->
          standardTestForWords ["ayayx", "byayx", "aybyx"] 8 8
      , test "what-phat" <|
        \_ ->
          standardTestForWords ["what", "phat"] 5 4
      , test "what's up-whotsup-wassup-whatsapp-wazzup" <|
        \_ ->
          standardTestForWords ["what's up", "whotsup", "wassup", "whatsapp", "wazzup"] 16 19
      , test "able-unable-disable" <|
        \_ ->
          standardTestForWords ["able", "unable", "disable"] 9 10
      , test "tap-tar-top" <|
        \_ ->
          standardTestForWords ["tap", "tar", "top"] 5 5
      , test "nation-action" <|
        \_ ->
          standardTestForWords ["nation", "action"] 8 8
      , test "nation-action-function" <|
        \_ ->
          standardTestForWords ["nation", "action", "function"] 10 11
      , test "nation-action-function-functionary" <|
        \_ ->
          standardTestForWords ["nation", "action", "function", "functionary"] 18 19
      , test "nation-action-function-functionary-native" <|
        \_ ->
          standardTestForWords ["nation", "action", "function", "functionary", "native"] 22 24
      , test "nation-function-functionary" <|
        \_ ->
          standardTestForWords ["nation", "function", "functionary"] 17 17
      , test "fred-freddy" <|
        \_ ->
          standardTestForWords ["fred", "freddy"] 7 6
      , test "fred-freddy-frederick" <|
        \_ ->
          standardTestForWords ["fred", "freddy", "frederick"] 11 11
      , test "nation-action-nativity-activity" <|
        \_ ->
          standardTestForWords ["nation", "action", "nativity", "activity"] 11 12
      , test "nation-action-nativity-activity-act" <|
        \_ ->
          standardTestForWords ["nation", "action", "nativity", "activity", "act"] 12 13
      , test "x-y" <|
        \_ ->
          standardTestForWords ["x", "y"] 2 1
      , test "tark-tavk" <|
        \_ ->
          standardTestForWords ["tark", "tavk"] 5 4
      , test "tark-turkey" <|
        \_ ->
          standardTestForWords ["tark", "turkey"] 9 9
      , test "tark-shark" <|
        \_ ->
          standardTestForWords ["tark", "shark"] 6 6
      , test "tar-tap" <|
        \_ ->
          standardTestForWords ["tar", "tap"] 4 3
      , test "task-tark" <|
        \_ ->
          standardTestForWords ["task", "tark"] 5 4
      , test "task-tark-tork" <|
        \_ ->
          standardTestForWords ["task", "tark", "tork"] 6 6
      , test "task-hork-terk" <|
        \_ ->
          standardTestForWords ["task", "hork", "terk"] 7 8
      , test "phong-pring" <|
        \_ ->
          standardTestForWords ["phong", "pring"] 7 7
      , test "phong-pring-pheng" <|
        \_ ->
          standardTestForWords ["phong", "pring", "pheng"] 7 7
      , test "prong-pring-pheng-prin" <|
        \_ ->
          standardTestForWords ["prong", "pring", "pheng", "prin"] 8 9
      , test "prong-pring-prin-phong" <|
        \_ ->
          standardTestForWords ["prong", "pring", "prin", "phong"] 8 9
      , test "tar-tap-box" <|
        \_ ->
          standardTestForWords ["tar", "tap", "box"] 6 6
      , test "try-pry-cry" <|
        \_ ->
          standardTestForWords ["try", "pry", "cry"] 4 3
      , test "aoeuth-aontuh-th-aoentuh-aoentuh-ouh" <|
        \_ ->
          standardTestForWords ["aoeuth", "aontuh", "th", "aoentuh", "aoentuh", "ouh"] 9 12
      , test "axax-bx-cx-cxax" <|
        \_ ->
          standardTestForWords ["axax", "bx", "cx", "cxax"] 6 7
      ]
    , describe "basic tests"
      -- Expect.equal is designed to be used in pipeline style, like this.
      [ test "adding a string to an empty graph gives us one word" <|
        \_ ->
          D.addString "a" D.empty
          |> nodesAndWords
          |> Expect.equal (2, ["a"])
      , test "an empty DAWG has no words in it" <|
        \_ ->
          (D.numNodes D.empty, D.recognizedWords D.empty)
          |> Expect.equal (1, [])
      , test "we can handle two totally separate words" <|
        \_ ->
          standardTestForWords ["abc", "def"] 6 6
      , test "adding a string to a graph with one vertex gives sequential transitions" <|
        \_ ->
          D.addString "ab" D.empty
          |> nodesAndWords
          |> Expect.equal (3, ["ab"])
      , test "adding a string to a graph with one vertex gives sequential transitions and another word" <|
        \_ ->
          D.addString "a" D.empty
          |> D.addString "ab"
          |> nodesAndWords
          |> Expect.equal (3, ["a", "ab"])
      , test "adding an empty word does nothing" <|
        \_ ->
          D.empty
          |> D.addString ""
          |> \d -> (D.numNodes d, D.recognizedWords d) -- note: not `verifiedRecognizedWords`
          |> Expect.equal (1, [])
      , test "adding a single letter works" <|
        \_ ->
          D.empty
          |> D.addString "😃"
          |> nodesAndWords
          |> Expect.equal (2, ["😃"])
      , test "adding two single letters works" <|
        \_ ->
          D.empty
          |> D.addString "ab"
          |> nodesAndWords
          |> Expect.equal (3, ["ab"])
      , test "adding multiple letters works" <|
        \_ ->
          D.empty
          |> D.addString "ghafūr"
          |> nodesAndWords
          |> Expect.equal (7, ["ghafūr"])
      , test "repeating an already existing string does nothing" <|
        \_ ->
          D.empty
          |> D.addString "kurremkarmerruk"
          |> D.addString "kurremkarmerruk" |> D.debugDAWG "check"
          |> nodesAndWords
          |> Expect.equal (16, ["kurremkarmerruk"])
      ]
    , describe "stress-testing via fuzzing"
      [ fuzz (Fuzz.listOfLengthBetween 2 8 (Fuzz.asciiStringOfLengthBetween 1 5)) "always recognizes exactly the unique short words that it is given" <|
        \listOfStrings ->
          D.fromWords (List.unique listOfStrings)
          |> D.verifiedRecognizedWords
          |> Expect.equal (List.sort <| List.unique listOfStrings)

      , fuzz (Fuzz.listOfLengthBetween 3 7 (Fuzz.asciiStringOfLengthBetween 5 8)) "always recognizes exactly the unique long words that it is given" <|
        \listOfStrings ->
          D.fromWords (List.unique listOfStrings)
          |> D.verifiedRecognizedWords
          |> Expect.equal (List.sort <| List.unique listOfStrings)

      , fuzz (Fuzz.listOfLengthBetween 3 5 (Fuzz.asciiStringOfLengthBetween 5 10)) "consistently returns the same dawg irrespective of the order in which words are entered" <|
        \listOfStrings ->
          let
            dawg0 = D.fromWords listOfStrings
            edges0 = D.numEdges dawg0
            nodes0 = D.numNodes dawg0
          in
            standardTestForWords listOfStrings nodes0 edges0

      , fuzz (Fuzz.asciiStringOfLengthBetween 1 65) "a string that is randomly generated works" <|
        \randomlyGeneratedString ->
          D.empty
          |> D.addString randomlyGeneratedString
          |> nodesAndWords
          |> Expect.equal (String.length randomlyGeneratedString + 1, [randomlyGeneratedString])
      ]
      , describe "cheaper one-shot tests; uncomment the relevant expensive test for a full workout"
        [
        test "pqt-zvt-zvr-pqr-pvt-zqr-pvr-zqt" <|
        \_ -> -- COMMENTED OUT: this will generate 40320 different combinations!!
          cheapTestForWords ["pqt", "zvt", "zvr", "pqr", "pvt", "zqr", "pvr", "zqt"] 4 3
      , test "towxm-tbwxm-tovxm-tbvxm-towym-tbwym-tovym-tbvym" <|
        \_ -> -- EXPENSIVE!!!  Generates 40320 permutations!
          cheapTestForWords ["towxm", "tbwxm", "tovxm", "tbvxm", "towym", "tbwym", "tovym", "tbvym"] 6 5
      , test "test-testable-tester-nest-nestable-nester-nestle" <|
        \_ ->
          cheapTestForWords ["test","testable","tester","nest","nestable","nester","nestle"] 14 17
      , test "test-testable-tester-nest-nestable-nester-ne" <|
        \_ ->
          cheapTestForWords ["test","testable","tester","nest","nestable","nester","ne"] 11 12
        ]
    -- , describe "really expensive tests"
    --   [
    --     test "pqt-zvt-zvr-pqr-pvt-zqr-pvr-zqt" <|
    --     \_ -> -- COMMENTED OUT: this will generate 40320 different combinations!!
    --       standardTestForWords ["pqt", "zvt", "zvr", "pqr", "pvt", "zqr", "pvr", "zqt"] 4 3
    --   , test "towxm-tbwxm-tovxm-tbvxm-towym-tbwym-tovym-tbvym" <|
    --     \_ -> -- EXPENSIVE!!!  Generates 40320 permutations!
    --       standardTestForWords ["towxm", "tbwxm", "tovxm", "tbvxm", "towym", "tbwym", "tovym", "tbvym"] 6 5
    --   , test "test-testable-tester-nest-nestable-nester-nestle" <|
    --     \_ ->
    --       standardTestForWords ["test","testable","tester","nest","nestable","nester","nestle"] 14 17
    --   , test "test-testable-tester-nest-nestable-nester-ne" <|
    --     \_ ->
    --       standardTestForWords ["test","testable","tester","nest","nestable","nester","ne"] 11 12
    --   ]
    ]