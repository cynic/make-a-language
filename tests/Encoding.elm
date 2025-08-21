module Encoding exposing (..)
import Test exposing (..)
import Utility exposing (..)
import Expect
import Automata.Data exposing (mkAG_input)

suite : Test
suite =
  describe "String to transitions parsing"
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
        Expect.equal [] (mkAG_input "1-x-1")
    -- , test "handles references" <|
    --   \_ ->
    --     Expect.equal

    ]