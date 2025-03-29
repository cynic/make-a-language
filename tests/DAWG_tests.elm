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

testWith : String -> Test
testWith s =
  test s <| \_ -> standardTestForWords (String.split "-" s)

testWithInputs : List String -> List Test
testWithInputs = List.map testWith

suite : Test
suite =
  describe "The DAWG module"
    [ -- Nest as many descriptions as you like.
    describe "handles construction correctly when given" <|
      testWithInputs [
        "px-pxa-pya-pya"
      , "pxa-py-qya"
      , "pxa-py-pya-pya"
      , "py-pya-pya"
      , "py-pya"
      , "px-pxa-pya-py"
      , "pya-py-py"
      , "py-py"
      , "pxxa-pyy-pyya"
      , "pxxa-pyya-py"
      , "pxaab-pya-pyaab"
      , "pxba-py-pyyba"
      , "pxba-py-pyba"
      , "pxa-py-pya-pr-pra"
      , "ps-s"
      , "aps-as"
      , "aps-ps"
      , "apqzs-as"
      , "apqzs-azs"
      , "ate-create"
      , "bars-ballad-bxrs"
      , "bats-bars-bxrs"
      , "naton-acton-natit-actit"
      , "aton-cton-atit-ctit"
      , "ato-cto-ati"
      , "ato-cto-at"
      , "ato-cto-ati-cti"
      , "ato-cto"
      , "pqt-zvt-pqr"
      , "pqt-zqr-pvr-pvt"
      , "pqt-prt-pqr-zvt"
      , "pqt-zvt-zvr-pqr"
      , "pqt-zvt-pqarcz-pqr"
      , "pqt-zvt-zvarc-pqr"
      , "pqt-zvt-pqrr"
      , "pqt-zvt-zvxt"
      , "x-y-xx"
      , "kxto-pzto-pzt"
      , "crv-ax-cx-arv"
      , "pato-qcto-pat"
      , "pato-qcto-pati"
      , "pato-peto-qcto-pati"
      , "ato-cto-atoz"
      , "pato-qcto-patoz"
      , "atpvnxb-ctpvnxb-atprxb-ctprxb-atprxy"
      , "xa-y-ya"
      , "xa-y-yaa"
      , "xa-y-yaaa"
      , "xa-y-yaaaa"
      , "av-rv-kv-kva"
      , "rxa-ry-pva-py-pxa"
      , "xav-pbt-pzv"
      , "park-qv-qsk"
      , "park-qv-qvrk"
      , "an-tn-x-tx"
      , "x-y-am-xm"
      , "x-b-bc-ac-bx"
      , "px-pb-pbc-pac-pbz"
      , "x-b-bc-ac-bzo"
      , "kp-gx-ax-gp"
      , "owx-bwx-ovx-bvx-owy"
      , "owx-bwx-ovx-bvx-bv"
      , "be-dv-cv-a-de"
      , "ttal-ntl-ntal"
      , "ttal-tyal-ntl-ntal"
      , "tt-ttal-nt-ntl-ntal"
      , "tstabl-nstabl-nstl"
      , "tsbl-nsbl-nsl"
      , "nst-nsl-tst-tsl-nstl"
      , "tst-tstabl-nst-nstabl-nstl"
      , "teste-ne-neste"
      , "tere-cere-te"
      , "teve-ceve-cyve-tyve-te"
      , "ayxpayx-byxpayx-ayxpbyx-byxpbyx-ayx"
      , "ayxpayx-byxpayx-ayxpbyx-byxpbyx-ayxpayx"
      , "ayayx-byayx-aybyx"
      , "what-phat"
      , "what'sup-whotsup-wassup-whatsapp-wazzup"
      , "able-unable-disable"
      , "tap-tar-top"
      , "nation-action"
      , "nation-action-function"
      , "nation-action-function-functionary"
      , "nation-action-function-functionary-native"
      , "nation-function-functionary"
      , "fred-freddy"
      , "fred-freddy-frederick"
      , "nation-action-nativity-activity"
      , "nation-action-nativity-activity-act"
      , "x-y"
      , "tark-tavk"
      , "tark-turkey"
      , "tark-shark"
      , "tar-tap"
      , "task-tark"
      , "task-tark-tork"
      , "task-hork-terk"
      , "phong-pring"
      , "phong-pring-pheng"
      , "prong-pring-pheng-prin"
      , "prong-pring-prin-phong"
      , "tar-tap-box"
      , "try-pry-cry"
      , "aoeuth-aontuh-th-aoentuh-aoentuh-ouh"
      , "axax-bx-cx-cxax"
      , "ad-ars-xrs"
      , "skop-rakop-rap"
      , "skoz-rakop-rap-raz"
      , "xaz-yaz-ytp"
      , "ad-da-dba"
      , "txl-tl"
      , "sy-spw-ow"
      , "aka-aman-aasn-abasn"
      , "hazsu-hosu-ssu-hap"
      , "pa-az-pxz"
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