module Jsonify exposing (..)
import Json.Encode as E
import Json.Decode as D
-- import GraphEditor exposing (..)
import Automata.Data exposing (..)
import Uuid
import Time
import Automata.DFA as DFA
import TypedSvg exposing (g)
import AutoDict
import Uuid exposing (Uuid)
import List.Extra as List
import Automata.Debugging exposing (debugLog_)

encodeTest : Test -> E.Value
encodeTest { input, expectation } =
  E.object
    [ ("input", E.string input)
    , ("expectation", E.bool (expectation == ExpectAccepted))
    -- do not store the result. It must be recalculated each time.
    ]

decodeTest : ResolutionDict -> AutomatonGraph -> D.Decoder Test
decodeTest resolutionDict g =
  D.map2
    (\input expectation ->
      { input = input
      , expectation = expectation
      , result =
          DFA.load input resolutionDict g
          |> DFA.run
          -- |> debugLog_ ("Results for '" ++ input ++ "'") (List.map .finalResult)
          |> List.head
          |> Maybe.andThen .finalResult
          |> Maybe.withDefault (InternalError "Failed to run test")
      })
    (D.field "input" D.string)
    (D.field "expectation" <| D.map (\b -> if b then ExpectAccepted else ExpectRejected) D.bool)

encodeGraphPackage : GraphPackage -> E.Value
encodeGraphPackage pkg =
  E.object
    [ ("graph", DFA.encodeAutomatonGraph pkg.userGraph)
    -- , ("dimensions",
    --     E.object
    --       [ ("w", E.float <| Tuple.first pkg.dimensions)
    --       , ("h", E.float <| Tuple.second pkg.dimensions)
    --       ]
    --   )
    , ("description", Maybe.map E.string pkg.description |> Maybe.withDefault E.null)
    , ("created", E.int (Time.posixToMillis pkg.created))
    , ("tests", DFA.encodeAutoDict Uuid.toString encodeTest pkg.tests)
    , ("currentTestKey", Uuid.encode pkg.currentTestKey)
    ]

decodeGraphPackage : ResolutionDict -> D.Decoder GraphPackage
decodeGraphPackage resolutionDict =
  D.field "graph" DFA.decodeAutomatonGraph
  |> D.andThen
    (\graph ->
      D.map4
        (\description created testKey tests ->
            GraphPackage graph description created testKey tests [] []
        )
        -- ( D.map2
        --     (\w h -> (w, h))
        --     (D.at ["dimensions", "w"] D.float)
        --     (D.at ["dimensions", "h"] D.float)
        -- )
        (D.field "description" <| D.oneOf [ D.null Nothing, D.map Just D.string ])
        (D.field "created" <| D.map Time.millisToPosix D.int)
        (D.field "currentTestKey" Uuid.decoder)
        (D.field "tests" <| DFA.decodeAutoDict Uuid.toString Uuid.fromString (decodeTest resolutionDict graph))
    )

decodeFlags : D.Decoder Flags
decodeFlags =
  D.map (\ag_list ->
    List.map (\ag -> (ag.graphIdentifier, ag)) ag_list
    |> AutoDict.fromList Uuid.toString
  )
  (D.field "packages" <| D.list (D.field "graph" DFA.decodeAutomatonGraph))
  |> D.andThen
    (\resolutionDict ->
      D.map2 (\w h -> (w, h))
        (D.field "width" D.float)
        (D.field "height" D.float)
      |> D.andThen
        (\(w, h) ->
          D.map4 (Flags w h)
            (D.field "initialSeed" D.int)
            (D.field "extendedSeeds" <| D.list D.int)
            (D.field "startTime" <| D.map Time.millisToPosix D.int)
            (D.field "packages" <| D.list (decodeGraphPackage resolutionDict))
        )
    )
