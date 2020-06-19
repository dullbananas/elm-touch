module BenchmarkTest exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner as Runner

import Touch
import Touch.Internal

import Math.Vector2 exposing (vec2)



main : Runner.BenchmarkProgram
main =
    Runner.program suite


model : Touch.Internal.Model ()
model =
    Touch.Internal.initModel []


event touches =
    { touches =
        List.map
            ( \( id, x, y ) ->
                { clientPos = vec2 x y
                , identifier = id
                }
            ) touches
    }
        |> Touch.Internal.Moved


update : Touch.Internal.Listener () -> Touch.Internal.Model ()
update listener =
    let
        newModel =
            { model | listeners = [
                { listener = listener }
            ] }

        msg1 =
            event
                [ ( 0, 101, 208 )
                ]

        msg2 =
            event
                [ ( 0, -51, 118 )
                , ( 1, 67, -65 )
                ]
    in
    Touch.Internal.update msg1 newModel identity
        |> Tuple.first
        |> ( \model2 -> Touch.Internal.update msg2 model2 identity )
        |> Tuple.first


suite : Benchmark
suite =
    let
        dummyModel =
            update <| Touch.Internal.OnRotate <| \_ -> ()
    in
    describe "Touch"
        [ benchmark "calcAngle" <| \_ ->
            Touch.Internal.calcAngle
                ( vec2 -24 98 )
                ( vec2 90 -42 )

        , benchmark "triggerMsgs" <| \_ ->
            Touch.Internal.triggerMsgs
                [ 9, 4, 7, 100, 1, 56 ]

        , benchmark "getTouchPositions" <| \_ ->
            dummyModel
                |> Touch.Internal.getTouchPositions

        , benchmark "onPinch" <| \_ ->
            update <| Touch.Internal.OnPinch <| \_ -> ()

        , benchmark "onRotate" <| \_ ->
            update <| Touch.Internal.OnRotate <| \_ -> ()

        , benchmark "onMove" <| \_ ->
            update <| Touch.Internal.OnMove { fingers = 2 } <| \_ _ -> ()
        ]
