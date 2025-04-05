module DAWG_benchmarks exposing (..)
import Array
import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import DAWG exposing (..)

suite : Benchmark
suite =
  let
    sampleArray =
        Array.initialize 100 identity
    fifty_words =
      [ "automaticity", "heartsomeness", "cisterna", "barometz", "unhealing", "rat's", "gloea", "drepanoid", "klystrons", "annulettee"
      , "sphenotemporal", "bromauric", "lecithin", "digitonin", "briefnesses", "tungstite", "signaling", "yclad", "astrophysicist's", "endoprocta", "myelobrachium"
      , "ceraunoscopy", "iconoclast", "nictitated", "dibbles", "stonewalls", "unpriced", "salal", "cephalothin", "mainlines", "indoctrinize"
      , "poetries", "pericowperitis", "registers", "veterinarian's", "oslo", "plural", "subchiefs", "thyrsoidal", "estranged", "wigwagged", "timeless"
      , "chasselas", "unfancy", "unrecurrent", "pikakes", "fistic", "raptorial", "unmisled", "bacterise"
      ]
  in
    describe "DAWG"
      [ describe "(Original Simplify) fromWords" <|
        [ benchmark "50 words" <|
            \_ -> fromWords fifty_words
        ]
      ]
    -- describe "Array"
    --   [ -- nest as many descriptions as you like
    --     describe "slice"
    --       [ benchmark "from the beginning" <|
    --           \_ -> Array.slice 50 100 sampleArray
    --       , benchmark "from the end" <|
    --           \_ -> Array.slice 0 50 sampleArray
    --       ]
    --   ]


main : BenchmarkProgram
main =
  program suite