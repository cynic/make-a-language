module DAWG_tests exposing (..)

import Expect
import Fuzz
import Test exposing (..)
import DAWG as D
import List.Extra as List

nodesAndWords : D.DAWG -> (Int, List String)
nodesAndWords d =
  (D.numNodes d, D.verifiedRecognizedWords d)

czech : List (List String) -> List String -> Expect.Expectation
czech l expectedRecognized =
  case l of
    [] ->
      Expect.pass
    x::rest ->
      let
        dawg =
          D.algebraToDAWG <| D.wordsToAlgebra x
        -- edges = D.numEdges dawg
        -- nodes = D.numNodes dawg
        minimality = D.minimality dawg
        recognized = D.verifiedRecognizedWords dawg
      in
        if recognized /= expectedRecognized then
          Debug.log "Failure on recognized words of permutation" x
          |> \_ -> Expect.equal recognized expectedRecognized
        -- else if nodes /= expectedNodes then
        --   Debug.log "Failure on node-count of permutation" x
        --   |> \_ -> Expect.equal nodes expectedNodes
        -- else if edges /= expectedEdges then
        --   Debug.log "Failure on edge-count of permutation" x
        --   |> \_ -> Expect.equal edges expectedEdges
        else if not (List.isEmpty minimality) then
          Debug.log "Failure on minimality" x
          |> \_ -> Expect.fail ("can combine: " ++ (List.map (\combinable -> "[" ++ (List.map String.fromInt combinable |> String.join ", ") ++ "]") minimality |> String.join "; "))
        else
          czech rest expectedRecognized

expectedWords : List String -> List String
expectedWords = List.sort >> List.unique >> List.filter ((/=) "")

standardTestForWords : List String -> Expect.Expectation
standardTestForWords words =
  czech (List.permutations words) (expectedWords words)

cheapTestForWords : List String -> Expect.Expectation
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
          standardTestForWords ["px", "pxa", "pya", "pya"]
      , test "pxa-py-qya" <|
        \_ ->
          standardTestForWords ["pxa", "py", "qya"]
      , test "pxa-py-pya-pya" <|
        \_ ->
          standardTestForWords ["pxa", "py", "pya", "pya"]
      , test "py-pya-pya" <|
        \_ ->
          standardTestForWords ["py", "pya", "pya"]
      , test "py-pya" <|
        \_ ->
          standardTestForWords ["py", "pya"]
      , test "px-pxa-pya-py" <|
        \_ ->
          standardTestForWords ["px", "pxa", "pya", "py"]
      , test "pya-py-py" <|
        \_ ->
          standardTestForWords ["pya", "py", "py"]
      , test "py-py" <|
        \_ ->
          standardTestForWords ["py", "py"]
      , test "pxxa-pyy-pyya" <|
        \_ ->
          standardTestForWords ["pxxa", "pyy", "pyya"]
      , test "pxxa-pyya-py" <|
        \_ ->
          standardTestForWords ["pxxa", "pyya", "py"]
      , test "pxaab-pya-pyaab" <|
        \_ ->
          standardTestForWords ["pxaab", "pya", "pyaab"]
      , test "pxba-py-pyyba" <|
        \_ ->
          standardTestForWords ["pxba", "py", "pyyba"]
      , test "pxba-py-pyba" <|
        \_ ->
          standardTestForWords ["pxba", "py", "pyba"]
      , test "pxa-py-pya-pr-pra" <|
        \_ ->
          standardTestForWords ["pxa", "py", "pya", "pr", "pra"]
      , test "ps-s" <|
        \_ ->
          standardTestForWords ["ps", "s"]
      , test "aps-as" <|
        \_ ->
          standardTestForWords ["aps", "as"]
      , test "aps-ps" <|
        \_ ->
          standardTestForWords ["aps", "ps"]
      , test "apqzs-as" <|
        \_ ->
          standardTestForWords ["apqzs", "as"]
      , test "apqzs-azs" <|
        \_ ->
          standardTestForWords ["apqzs", "azs"]
      , test "ate-create" <|
        \_ ->
          standardTestForWords ["ate", "create"]
      , test "bars-ballad-bxrs" <|
        \_ ->
          standardTestForWords ["bars", "ballad", "bxrs"]
      , test "bats-bars-bxrs" <|
        \_ ->
          standardTestForWords ["bats", "bars", "bxrs"]
      , test "naton-acton-natit-actit" <|
        \_ ->
          standardTestForWords ["naton", "acton", "natit", "actit"]
      , test "aton-cton-atit-ctit" <|
        \_ ->
          standardTestForWords ["aton", "cton", "atit", "ctit"]
      , test "ato-cto-ati" <|
        \_ ->
          standardTestForWords ["ato", "cto", "ati"]
      , test "ato-cto-at" <|
        \_ ->
          standardTestForWords ["ato", "cto", "at"]
      , test "ato-cto-ati-cti" <|
        \_ ->
          standardTestForWords ["ato", "cto", "ati", "cti"]
      , test "ato-cto" <|
        \_ ->
          standardTestForWords ["ato", "cto"]
      , test "pqt-zvt-pqr" <|
        \_ ->
          standardTestForWords ["pqt", "zvt", "pqr"]
      , test "pqt-zqr-pvr-pvt" <|
        \_ ->
          standardTestForWords ["pqt", "zqr", "pvr", "pvt"]
      , test "pqt-prt-pqr-zvt" <|
        \_ ->
          standardTestForWords ["pqt", "prt", "pqr", "zvt"]
      , test "pqt-zvt-zvr-pqr" <|
        \_ ->
          standardTestForWords ["pqt", "zvt", "zvr", "pqr"]
      , test "pqt-zvt-pqarcz-pqr" <|
        \_ ->
          standardTestForWords ["pqt", "zvt", "pqarcz", "pqr"]
      , test "pqt-zvt-zvarc-pqr" <|
        \_ ->
          standardTestForWords ["pqt", "zvt", "zvarc", "pqr"]
      , test "pqt-zvt-pqrr" <|
        \_ ->
          standardTestForWords ["pqt", "zvt", "pqrr"]
      , test "pqt-zvt-zvxt" <|
        \_ ->
          standardTestForWords ["pqt", "zvt", "zvxt"]
      , test "x-y-xx" <|
        \_ ->
          standardTestForWords ["x", "y", "xx"]
      , test "kxto-pzto-pzt" <|
        \_ ->
          standardTestForWords ["kxto", "pzto", "pzt"]
      , test "crv-ax-cx-arv" <|
        \_ ->
          standardTestForWords ["crv", "ax", "cx", "arv"]
      , test "pato-qcto-pat" <|
        \_ ->
          standardTestForWords ["pato", "qcto", "pat"]
      , test "pato-qcto-pati" <|
        \_ ->
          standardTestForWords ["pato", "qcto", "pati"]
      , test "pato-peto-qcto-pati" <|
        \_ ->
          standardTestForWords ["pato", "peto", "qcto", "pati"]
      , test "ato-cto-atoz" <|
        \_ ->
          standardTestForWords ["ato", "cto", "atoz"]
      , test "pato-qcto-patoz" <|
        \_ ->
          standardTestForWords ["pato", "qcto", "patoz"]
      , test "atpvnxb-ctpvnxb-atprxb-ctprxb-atprxy" <|
        \_ ->
          standardTestForWords ["atpvnxb", "ctpvnxb", "atprxb", "ctprxb", "atprxy"]
      , test "xa-y-ya" <|
        \_ ->
          standardTestForWords ["xa", "y", "ya"]
      , test "xa-y-yaa" <|
        \_ ->
          standardTestForWords ["xa", "y", "yaa"]
      , test "xa-y-yaaa" <|
        \_ ->
          standardTestForWords ["xa", "y", "yaaa"]
      , test "xa-y-yaaaa" <|
        \_ ->
          standardTestForWords ["xa", "y", "yaaaa"]
      , test "av-rv-kv-kva" <|
        \_ ->
          standardTestForWords ["av", "rv", "kv", "kva"]
      , test "rxa-ry-pva-py-pxa" <|
        \_ ->
          standardTestForWords ["rxa", "ry", "pva", "py", "pxa"]
      , test "xav-pbt-pzv" <|
        \_ ->
          standardTestForWords ["xav", "pbt", "pzv"]
      , test "park-qv-qsk" <|
        \_ ->
          standardTestForWords ["park", "qv", "qsk"]
      , test "park-qv-qvrk" <|
        \_ ->
          standardTestForWords ["park", "qv", "qvrk"]
      , test "an-tn-x-tx" <|
        \_ ->
          standardTestForWords ["an", "tn", "x", "tx"]
      , test "x-y-am-xm" <|
        \_ ->
          standardTestForWords ["x", "y", "am", "xm"]
      , test "x-b-bc-ac-bx" <|
        \_ ->
          standardTestForWords ["x", "b", "bc", "ac", "bx"]
      , test "px-pb-pbc-pac-pbz" <|
        \_ ->
          standardTestForWords ["px", "pb", "pbc", "pac", "pbz"]
      , test "x-b-bc-ac-bzo" <|
        \_ ->
          standardTestForWords ["x", "b", "bc", "ac", "bzo"]
      , test "kp-gx-ax-gp" <|
        \_ ->
          standardTestForWords ["kp","gx","ax","gp"]
      , test "owx-bwx-ovx-bvx-owy" <|
        \_ ->
          standardTestForWords ["owx","bwx","ovx","bvx","owy"]
      , test "owx-bwx-ovx-bvx-bv" <|
        \_ ->
          standardTestForWords ["owx","bwx","ovx","bvx","bv"]
      , test "be-dv-cv-a-de" <|
        \_ ->
          standardTestForWords ["be","dv","cv","a","de"]
      , test "ttal-ntl-ntal" <|
        \_ ->
          standardTestForWords ["ttal","ntl","ntal"]
      , test "ttal-tyal-ntl-ntal" <|
        \_ ->
          standardTestForWords ["ttal","tyal","ntl","ntal"]
      , test "tt-ttal-nt-ntl-ntal" <|
        \_ ->
          standardTestForWords ["tt","ttal","nt","ntl","ntal"]
      , test "tstabl-nstabl-nstl" <|
        \_ ->
          standardTestForWords ["tstabl","nstabl","nstl"]
      , test "tsbl-nsbl-nsl" <|
        \_ ->
          standardTestForWords ["tsbl","nsbl","nsl"]
      , test "nst-nsl-tst-tsl-nstl" <|
        \_ ->
          standardTestForWords ["nst", "nsl", "tst", "tsl", "nstl"]
      , test "tst-tstabl-nst-nstabl-nstl" <|
        \_ ->
          standardTestForWords ["tst","tstabl","nst","nstabl","nstl"]
      , test "teste-ne-neste" <|
        \_ ->
          standardTestForWords ["teste", "ne", "neste"]
      , test "tere-cere-te" <|
        \_ ->
          standardTestForWords ["tere","cere","te"]
      , test "teve-ceve-cyve-tyve-te" <|
        \_ ->
          standardTestForWords ["teve", "ceve", "cyve", "tyve", "te"]
      , test "ayxpayx-byxpayx-ayxpbyx-byxpbyx-ayx" <|
        \_ ->
          standardTestForWords ["ayxpayx", "byxpayx", "ayxpbyx", "byxpbyx", "ayx"]
      , test "ayxpayx-byxpayx-ayxpbyx-byxpbyx-ayxpayx" <|
        \_ ->
          standardTestForWords ["ayxpayx", "byxpayx", "ayxpbyx", "byxpbyx", "ayxpayx"]
      , test "ayayx-byayx-aybyx" <|
        \_ ->
          standardTestForWords ["ayayx", "byayx", "aybyx"]
      , test "what-phat" <|
        \_ ->
          standardTestForWords ["what", "phat"]
      , test "what'sup-whotsup-wassup-whatsapp-wazzup" <|
        \_ ->
          standardTestForWords ["what'sup", "whotsup", "wassup", "whatsapp", "wazzup"]
      , test "able-unable-disable" <|
        \_ ->
          standardTestForWords ["able", "unable", "disable"]
      , test "tap-tar-top" <|
        \_ ->
          standardTestForWords ["tap", "tar", "top"]
      , test "nation-action" <|
        \_ ->
          standardTestForWords ["nation", "action"]
      , test "nation-action-function" <|
        \_ ->
          standardTestForWords ["nation", "action", "function"]
      , test "nation-action-function-functionary" <|
        \_ ->
          standardTestForWords ["nation", "action", "function", "functionary"]
      , test "nation-action-function-functionary-native" <|
        \_ ->
          standardTestForWords ["nation", "action", "function", "functionary", "native"]
      , test "nation-function-functionary" <|
        \_ ->
          standardTestForWords ["nation", "function", "functionary"]
      , test "fred-freddy" <|
        \_ ->
          standardTestForWords ["fred", "freddy"]
      , test "fred-freddy-frederick" <|
        \_ ->
          standardTestForWords ["fred", "freddy", "frederick"]
      , test "nation-action-nativity-activity" <|
        \_ ->
          standardTestForWords ["nation", "action", "nativity", "activity"]
      , test "nation-action-nativity-activity-act" <|
        \_ ->
          standardTestForWords ["nation", "action", "nativity", "activity", "act"]
      , test "x-y" <|
        \_ ->
          standardTestForWords ["x", "y"]
      , test "tark-tavk" <|
        \_ ->
          standardTestForWords ["tark", "tavk"]
      , test "tark-turkey" <|
        \_ ->
          standardTestForWords ["tark", "turkey"]
      , test "tark-shark" <|
        \_ ->
          standardTestForWords ["tark", "shark"]
      , test "tar-tap" <|
        \_ ->
          standardTestForWords ["tar", "tap"]
      , test "task-tark" <|
        \_ ->
          standardTestForWords ["task", "tark"]
      , test "task-tark-tork" <|
        \_ ->
          standardTestForWords ["task", "tark", "tork"]
      , test "task-hork-terk" <|
        \_ ->
          standardTestForWords ["task", "hork", "terk"]
      , test "phong-pring" <|
        \_ ->
          standardTestForWords ["phong", "pring"]
      , test "phong-pring-pheng" <|
        \_ ->
          standardTestForWords ["phong", "pring", "pheng"]
      , test "prong-pring-pheng-prin" <|
        \_ ->
          standardTestForWords ["prong", "pring", "pheng", "prin"]
      , test "prong-pring-prin-phong" <|
        \_ ->
          standardTestForWords ["prong", "pring", "prin", "phong"]
      , test "tar-tap-box" <|
        \_ ->
          standardTestForWords ["tar", "tap", "box"]
      , test "try-pry-cry" <|
        \_ ->
          standardTestForWords ["try", "pry", "cry"]
      , test "aoeuth-aontuh-th-aoentuh-aoentuh-ouh" <|
        \_ ->
          standardTestForWords ["aoeuth", "aontuh", "th", "aoentuh", "aoentuh", "ouh"]
      , test "axax-bx-cx-cxax" <|
        \_ ->
          standardTestForWords ["axax", "bx", "cx", "cxax"]
      , test "ad-ars-xrs" <|
        \_ ->
          standardTestForWords ["ad", "ars", "xrs"]
      , test "skop-rakop-rap" <|
        \_ ->
          standardTestForWords ["skop", "rakop", "rap"]
      , test "skoz-rakop-rap-raz" <|
        \_ ->
          standardTestForWords ["skoz", "rakop", "rap", "raz"]
      , test "xaz-yaz-ytp" <|
        \_ ->
          standardTestForWords ["xaz", "yaz", "ytp"]
      , test "ad-da-dba" <|
        \_ ->
          standardTestForWords ["ad", "da", "dba"]
      , test "txl-tl" <|
        \_ ->
          standardTestForWords ["txl", "tl"]
      , test "sy-spw-ow" <|
        \_ ->
          standardTestForWords ["sy", "spw", "ow"]
      , test "aka-aman-aasn-abasn" <|
        \_ ->
          standardTestForWords ["aka", "aman", "aasn", "abasn"]
      ]
    , describe "stress-testing via fuzzing"
      [ fuzz (Fuzz.listOfLengthBetween 2 8 (Fuzz.asciiStringOfLengthBetween 1 5)) "always recognizes exactly the unique short words that it is given" <|
          cheapTestForWords
        -- \listOfStrings ->
        --   D.fromWords (List.unique listOfStrings)
        --   |> D.verifiedRecognizedWords
        --   |> Expect.equal (List.sort <| List.unique listOfStrings)

      , fuzz (Fuzz.listOfLengthBetween 3 7 (Fuzz.asciiStringOfLengthBetween 5 8)) "always recognizes exactly the unique long words that it is given" <|
          cheapTestForWords
        -- \listOfStrings ->
          -- D.fromWords (List.unique listOfStrings)
          -- |> D.verifiedRecognizedWords
          -- |> Expect.equal (List.sort <| List.unique listOfStrings)

      , fuzz (Fuzz.listOfLengthBetween 3 15 (Fuzz.asciiStringOfLengthBetween 1 10)) "always recognizes exactly the unique short or long words that it is given" <|
          cheapTestForWords

      , fuzz (Fuzz.listOfLengthBetween 3 5 (Fuzz.asciiStringOfLengthBetween 5 10)) "consistently returns the same dawg irrespective of the order in which words are entered" <|
          standardTestForWords

      , fuzz (Fuzz.asciiStringOfLengthBetween 1 65) "a string that is randomly generated works" <|
          \s -> cheapTestForWords [s]
        -- \randomlyGeneratedString ->
        --   D.empty
        --   |> D.addString randomlyGeneratedString
        --   |> nodesAndWords
        --   |> Expect.equal (String.length randomlyGeneratedString + 1, [randomlyGeneratedString])
      ]
      , describe "cheaper one-shot tests; uncomment the relevant expensive test for a full workout"
        [
          test "pqt-zvt-zvr-pqr-pvt-zqr-pvr-zqt" <|
          \_ -> -- COMMENTED OUT: this will generate 40320 different combinations!!
            cheapTestForWords ["pqt", "zvt", "zvr", "pqr", "pvt", "zqr", "pvr", "zqt"]
        , test "towxm-tbwxm-tovxm-tbvxm-towym-tbwym-tovym-tbvym" <|
          \_ -> -- EXPENSIVE!!!  Generates 40320 permutations!
            cheapTestForWords ["towxm", "tbwxm", "tovxm", "tbvxm", "towym", "tbwym", "tovym", "tbvym"]
        , test "test-testable-tester-nest-nestable-nester-nestle" <|
          \_ ->
            cheapTestForWords ["test","testable","tester","nest","nestable","nester","nestle"]
        , test "test-testable-tester-nest-nestable-nester-ne" <|
          \_ ->
            cheapTestForWords ["test","testable","tester","nest","nestable","nester","ne"]
        , test "sed-sedy-ses-td-ts-ots-op-ops-ds" <|
          \_ ->
            cheapTestForWords ["sed", "sedy", "ses", "td", "ts", "ots", "op", "ops", "ds"]
        ]
    -- , describe "really expensive tests"
    --   [
    --     test "pqt-zvt-zvr-pqr-pvt-zqr-pvr-zqt" <|
    --     \_ -> -- COMMENTED OUT: this will generate 40320 different combinations!!
    --       standardTestForWords ["pqt", "zvt", "zvr", "pqr", "pvt", "zqr", "pvr", "zqt"]
    --   , test "towxm-tbwxm-tovxm-tbvxm-towym-tbwym-tovym-tbvym" <|
    --     \_ -> -- EXPENSIVE!!!  Generates 40320 permutations!
    --       standardTestForWords ["towxm", "tbwxm", "tovxm", "tbvxm", "towym", "tbwym", "tovym", "tbvym"]
    --   , test "test-testable-tester-nest-nestable-nester-nestle" <|
    --     \_ ->
    --       standardTestForWords ["test","testable","tester","nest","nestable","nester","nestle"]
    --   , test "test-testable-tester-nest-nestable-nester-ne" <|
    --     \_ ->
    --       standardTestForWords ["test","testable","tester","nest","nestable","nester","ne"]
    --   ]
    ]