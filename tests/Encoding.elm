module Encoding exposing (..)
import Test exposing (..)
import Utility exposing (..)
import Expect
import Automata.Data exposing (mkAG_input, NodeEffect(..), AcceptVia(..), Entity)
import Automata.DFA exposing (serializeAutomatonGraph, mkConn, deserializeAutomatonGraph)
import Graph exposing (Node, Edge)
import Json.Decode as D

-- mkNode : Int -> Graph.Node Entity
mkNode id =
  Node id <| Entity 0 0 0 0 id NoEffect

mkEdge from to chars =
  Edge from to (mkConn chars)

mkAutomatonGraph nodeIds edges root =
  { graph =
      Graph.fromNodesAndEdges
        ( nodeIds |> List.map mkNode )
        ( List.map (\(from, to, chars) -> Edge from to (mkConn chars)) edges )
  , maxId = List.maximum nodeIds |> Maybe.withDefault 0
  , root = root
  }


suite : Test
suite =
  describe "Encoding & Decoding serialized automata"
    [ describe "Decoding: serialized string to AutomatonGraph"
      [ test "parses simple transitions correctly" <|
        \_ ->
          Expect.equal
            [(0, "!av", 1), (0, "b!vk!z", 2), (2, "p", 0)]
            (mkAG_input "0-!av-1 0-b!vk!z-2 2-p-0")
      , test "parses single transition" <|
        \_ ->
          Expect.equal
            [(1, "abc", 2)]
            (mkAG_input "1-abc-2")
      , test "handles empty string" <|
        \_ ->
          Expect.equal [] (mkAG_input "")
      , test "handles whitespace" <|
        \_ ->
          Expect.equal [(1, " ", 2)] (mkAG_input "1- -2")
      , test "handles a + character" <|
        \_ ->
          Expect.equal [(1, "+", 2)] (mkAG_input "1-++-2")
      ]
    , describe "Encoding: can round-trip AutomatonGraph"
      [ test "encodes simple transitions correctly" <|
        \_ ->
          let
            v =
              mkAutomatonGraph
                [0, 1, 2]
                [ (0, 1, "!av")
                , (0, 2, "b!vk!z")
                , (2, 0, "p")
                ]
                0
          in
            case serializeAutomatonGraph v |> D.decodeValue deserializeAutomatonGraph of
              Ok v_ -> Expect.equal v v_
              Err e -> Expect.fail (Debug.toString e)
      ]
    -- , test "handles references" <|
    --   \_ ->
    --     Expect.equal

    ]