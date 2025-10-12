module DFA_tests exposing (..)

import Expect
import Fuzz
import Test exposing (..)
import Automata.Data as D
import Automata.Verification as D
import List.Extra as List
import Automata.DFA
import Automata.DFA exposing (toAutomatonGraph, union)
import IntDict
import Set
import AutoDict
import Utility
import Array

-- nodesAndWords : D.AutomatonGraph -> (Int, List String)
-- nodesAndWords d =
--   (D.numNodes d, D.verifiedRecognizedWords d)


{-| Create a DFA that accepts exactly one string. -}
add_string_to_dfa : String -> Maybe (D.DFARecord {})
add_string_to_dfa string =
  if String.isEmpty string then
    Nothing
  else
    Just -- <| debugDFA_ ("Creating single-string DFA for '" ++ string ++ "'") <|
      -- If this is a n-character string, then we will want n+1 states
      { states =
          List.range 0 (String.length string)
          |> List.map (\i -> (i, Utility.dummyEntity i))
          |> IntDict.fromList
      , start = 0
      , finals = Set.singleton (String.length string)
      , transition_function =
          IntDict.fromList
            ( case String.toList string of
                [] ->
                  []
                [ch] ->
                  List.singleton
                    ( 0
                    , AutoDict.singleton D.acceptConditionToString (D.ViaCharacter ch) 1
                    )
                xs ->
                  List.foldl
                    (\ch (acc, nodeId) ->
                      ( (nodeId, AutoDict.singleton D.acceptConditionToString (D.ViaCharacter ch) (nodeId + 1)) :: acc
                      , nodeId + 1 ))
                    ( [], 0 )
                    xs
                  |> Tuple.first
            )
      }

addString : String -> Maybe (D.DFARecord {}) -> Maybe (D.DFARecord {})
addString string maybe_dfa =
  let
    string_dfa = add_string_to_dfa string
  in
    case ( maybe_dfa, string_dfa ) of
      ( Nothing, _ ) ->
        string_dfa
      ( _, Nothing ) ->
        maybe_dfa
      ( Just dfa, Just s ) ->
        Just (Automata.DFA.union s dfa {- |> debugDFA_ ("after adding '" ++ string ++ "'") -})

fromWords : List String -> D.DFARecord {}
fromWords =
  List.foldl addString Nothing
  >> Maybe.withDefault (Automata.DFA.empty (Utility.dummyEntity 0))
  -- >> Maybe.map toGraph
  -- >> Maybe.withDefault Automata.Data.empty

czech : List (List String) -> List String -> Expect.Expectation
czech l expectedRecognized =
  case l of
    [] ->
      Expect.pass
    x::rest ->
      let
        dfa =
          -- D.algebraToDAWG <| D.wordsToAlgebra x
          fromWords x
        minimality = D.minimality (toAutomatonGraph Utility.dummy_uuid dfa)
        -- recognized = D.verifiedRecognizedWords (toAutomatonGraph Utility.dummy_uuid dfa)
      in
        -- if recognized /= expectedRecognized then
        --   Debug.log "Failure on recognized words of permutation" x
        --   |> \_ -> Expect.equal recognized expectedRecognized
        --else
        if not (List.isEmpty minimality) then
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

dfaGenerator : Int -> Int -> Fuzz.Fuzzer (D.DFARecord {})
dfaGenerator random_conns_max nodes_max =
  let
    transition_chars =
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
      |> String.toList
    -- num_nodes = 8
    -- num_random_conns = 8
  in
    (Fuzz.pair (Fuzz.intRange 2 random_conns_max) (Fuzz.intRange 3 nodes_max))
    |> Fuzz.andThen
      (\(num_random_conns, num_nodes) ->
          Fuzz.map2
            (\destNodes finals ->
              let
                randoms =
                  List.map3
                    (\src ch dest -> ( src, ch, dest ))
                    (List.range 1 num_nodes) transition_chars destNodes
                links =
                  List.map2
                    (\src ch -> ( src, ch, src + 1 ))
                    (List.range 1 (num_nodes - 1)) (List.drop num_nodes transition_chars)
              in
                Utility.mkDFA (randoms ++ links) finals
            )
            (Fuzz.listOfLength num_random_conns (Fuzz.intRange 1 num_nodes))
            -- remember, node 1 can never be a final, when converting from AG…)
            (Fuzz.listOfLengthBetween 1 num_nodes (Fuzz.intRange 2 num_nodes))
      )

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
      , "s-ps-as-pss-pp"
      , "s-ps-aps-as"
      , "nest-nestable-nester-test-testable-tester"
      ]
    , describe "stress-testing via fuzzing"
      [ fuzz (Fuzz.listOfLengthBetween 2 8 (Fuzz.asciiStringOfLengthBetween 1 5)) "always recognizes exactly the unique short words that it is given" <|
          cheapTestForWords

      , fuzz (Fuzz.listOfLengthBetween 3 7 (Fuzz.asciiStringOfLengthBetween 5 8)) "always recognizes exactly the unique long words that it is given" <|
          cheapTestForWords

      , fuzz (Fuzz.listOfLengthBetween 3 15 (Fuzz.asciiStringOfLengthBetween 1 10)) "always recognizes exactly the unique short or long words that it is given" <|
          cheapTestForWords

      -- causes out-of-memory… so, weakening it…
      -- , fuzz (Fuzz.listOfLengthBetween 2 5 (Fuzz.asciiStringOfLengthBetween 4 8)) "consistently returns the same dawg irrespective of the order in which words are entered" <|
      --     standardTestForWords
      , fuzz (Fuzz.listOfLengthBetween 2 4 (Fuzz.asciiStringOfLengthBetween 3 6)) "consistently returns the same graph irrespective of the order in which words are entered" <|
          standardTestForWords

      , fuzz (Fuzz.asciiStringOfLengthBetween 1 65) "a string that is randomly generated works" <|
          \s -> cheapTestForWords [s]
      ]
      , describe "union tests"
        [
          test "a recursive word is unioned correctly" <|
          \_ ->
            let
              dfa1 = Utility.mkDFA [ (0, 'a', 1), (1, 'b', 2), (2, 'a', 1)  ] [1]
              dfa2 = Utility.mkDFA [ (0, 'a', 1), (1, 'b', 2), (2, 'd', 3) ] [3]
            in
              Utility.ag_equals
                (toAutomatonGraph Utility.dummy_uuid (union dfa1 dfa2))
                (toAutomatonGraph Utility.dummy_uuid (union dfa2 dfa1))
        , test "nodes are rechecked as necessary, after other nodes are processed" <|
          \_ ->
            let
              dfa1 = Utility.mkDFA [ (1, 'a', 2), (1, 'e', 2), (2, 'b', 4), (2, 'f', 3), (3, 'g', 4)  ] [2,4]
              dfa2 = Utility.mkDFA [ (6, 'a', 6), (6, 'e', 7), (7, 'b', 6), (7, 'f', 8), (8, 'g', 9) ] [7,9]
            in
              Utility.ag_equals
                (toAutomatonGraph Utility.dummy_uuid (union dfa1 dfa2))
                (toAutomatonGraph Utility.dummy_uuid (union dfa2 dfa1))
        , test "process nodes only when necessary" <|
          \_ ->
            let
              dfa1 = Utility.mkDFA [ (1, 'a', 1), (1, 'e', 2), (2, 'b', 3), (2, 'f', 3), (3, 'g', 4) ] [2]
              dfa2 = Utility.mkDFA [ (6, 'a', 7), (6, 'e', 7), (7, 'b', 8), (7, 'f', 8), (8, 'g', 9) ] [7,9]
            in
              Utility.ag_equals
                (toAutomatonGraph Utility.dummy_uuid (union dfa1 dfa2))
                (toAutomatonGraph Utility.dummy_uuid (union dfa2 dfa1))
        , test "nodes should be processed in order of whether they are 'last', so that right-language is preserved" <|
          \_ ->
            let
              dfa1 = Utility.mkDFA [ (1, 'a', 3), (1, 'e', 2), (2, 'b', 4), (2, 'f', 3), (3, 'g', 4) ] [2]
              dfa2 = Utility.mkDFA [ (6, 'a', 6), (6, 'e', 7), (7, 'b', 9), (7, 'f', 8), (8, 'g', 9) ] [9]
            in
              Utility.ag_cmp_and_equals
                (toAutomatonGraph Utility.dummy_uuid (union dfa1 dfa2))
                (toAutomatonGraph Utility.dummy_uuid (union dfa2 dfa1))
        , test "recursive references should be regarded as equivalent if their `via` matches" <|
          \_ ->
            let
              dfa1 = Utility.mkDFA [ (5, 'a', 6), (5, 'd', 6), (6, 'b', 6), (6, 'e', 7) ] [6,7]
              dfa2 = Utility.mkDFA [ (1, 'a', 3), (1, 'd', 2), (2, 'b', 2), (2, 'e', 3) ] [3]
            in
              Utility.ag_cmp_and_equals
                (toAutomatonGraph Utility.dummy_uuid (union dfa1 dfa2))
                (toAutomatonGraph Utility.dummy_uuid (union dfa2 dfa1))
        , test "minimality edge-case 2" <|
          \_ ->
            let
              dfa1 = Utility.mkDFA [ (1, 'a', 2), (1, 'd', 2), (2, 'b', 1), (2, 'e', 3) ] [2,3]
              dfa2 = Utility.mkDFA [ (5, 'a', 7), (5, 'd', 6), (6, 'b', 5), (6, 'e', 7) ] [6,7]
            in
              Utility.ag_cmp_and_equals
                (toAutomatonGraph Utility.dummy_uuid (union dfa1 dfa2))
                (toAutomatonGraph Utility.dummy_uuid (union dfa2 dfa1))
        , test "minimality edge-case 3" <|
          \_ ->
            let
              dfa1 = Utility.mkDFA [ (1, 'a', 1), (1, 'd', 2), (2, 'b', 1), (2, 'e', 3) ] [2]
              dfa2 = Utility.mkDFA [ (5, 'a', 7), (5, 'd', 6), (6, 'b', 5), (6, 'e', 7) ] [6]
            in
              Utility.ag_cmp_and_equals
                (toAutomatonGraph Utility.dummy_uuid (union dfa1 dfa2))
                (toAutomatonGraph Utility.dummy_uuid (union dfa2 dfa1))
        , test "minimality edge-case 4" <|
          \_ ->
            let
              dfa1 = Utility.mkDFA [ (1, 'a', 1), (1, 'd', 2), (2, 'b', 1), (2, 'e', 3) ] [2]
              dfa2 = Utility.mkDFA [ (5, 'a', 5), (5, 'd', 6), (6, 'b', 7), (6, 'e', 7) ] [6]
            in
              Utility.ag_cmp_and_equals
                (toAutomatonGraph Utility.dummy_uuid (union dfa1 dfa2))
                (toAutomatonGraph Utility.dummy_uuid (union dfa2 dfa1))
        , fuzz (Fuzz.pair (dfaGenerator 2 4) (dfaGenerator 2 4)) "DFA union is commutative (simple tests)" <|
          \(dfa1, dfa2) ->
            Utility.ag_cmp_and_equals
              (toAutomatonGraph Utility.dummy_uuid (union dfa1 dfa2))
              (toAutomatonGraph Utility.dummy_uuid (union dfa2 dfa1))
        , fuzz (Fuzz.pair (dfaGenerator 3 6) (dfaGenerator 3 6)) "DFA union is commutative (advanced tests)" <|
          \(dfa1, dfa2) ->
            Utility.ag_cmp_and_equals
              (toAutomatonGraph Utility.dummy_uuid (union dfa1 dfa2))
              (toAutomatonGraph Utility.dummy_uuid (union dfa2 dfa1))
        , fuzz (Fuzz.pair (dfaGenerator 25 10) (dfaGenerator 25 10)) "DFA union is commutative (stress tests)" <|
          \(dfa1, dfa2) ->
            Utility.ag_cmp_and_equals
              (toAutomatonGraph Utility.dummy_uuid (union dfa1 dfa2))
              (toAutomatonGraph Utility.dummy_uuid (union dfa2 dfa1))
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