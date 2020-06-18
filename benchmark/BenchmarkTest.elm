module BenchmarkTest exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner as Runner

import Touch
import Touch.Internal


main : Runner.BenchmarkProgram
main =
    Runner.program suite


suite : Benchmark
suite =
    describe "Touch"
        [ benchmark "calcAngle" <| \_ ->
            Touch.Internal.calcAngle
                { x = -24, y = 98 }
                { x = 90, y = -42 }
        ]
