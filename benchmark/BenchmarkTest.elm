module BenchmarkTest exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner as Runner

import Touch
import Touch.Internal


main : Runner.BenchmarkProgram
main =
    Runner.program suite


model : Touch.Internal.Model ()
model =
    Touch.Internal.initModel []


event touches =
    { keys =
        { alt = False, ctrl = False, shift = False }
    , changedTouches = []
    , targetTouches = []
    , touches =
        List.map
            ( \( id, x, y ) ->
                { identifier = id
                , clientPos = ( x, y )
                , pagePos = ( x, y )
                , screenPos = ( x, y )
                }
            ) touches
    }
        |> Touch.Internal.Event


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
    describe "Touch"
        [ benchmark "calcAngle" <| \_ ->
            Touch.Internal.calcAngle
                { x = -24, y = 98 }
                { x = 90, y = -42 }

        , benchmark "onPinch" <| \_ ->
            update <| Touch.Internal.OnPinch <| \_ -> ()

        , benchmark "onRotate" <| \_ ->
            update <| Touch.Internal.OnRotate <| \_ -> ()

        , benchmark "onMove" <| \_ ->
            update <| Touch.Internal.OnMove { fingers = 2 } <| \_ _ -> ()
        ]
