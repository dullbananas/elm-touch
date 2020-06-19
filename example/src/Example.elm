module Example exposing (main)


import Html exposing (..)
import Html.Attributes exposing (..)
import Browser
import Touch
import Time



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = always ( initModel, Touch.initCmd TouchMsg )
        , update = update
        , view = view
        , subscriptions = always <| Sub.batch
            [ Time.every 1000 ( always UpdateFps )
            ]
        }



-- Model


type alias Model =
    { touchModel : Touch.Model Msg
    , x : Float
    , y : Float
    , pinch : Float
    , radians : Float
    , fps : Int
    , nextFps : Int
    , vx : Float
    , vy : Float
    }


initModel : Model
initModel =
    { touchModel =
        Touch.initModel
            [ Touch.onMove { fingers = 2 } MovedTwoFingers
            , Touch.onPinch Pinched
            , Touch.onRotate Rotated
            ]
            [ Touch.getEndVelocity GotFinalVelocity
            ]
    , x = 0
    , y = 0
    , pinch = 0
    , radians = 0
    , fps = 0
    , nextFps = 0
    , vx = 0
    , vy = 0
    }



-- Update


type Msg
    = TouchMsg Touch.Msg

    | MovedTwoFingers Float Float
    | Pinched Float
    | Rotated Float
    | GotFinalVelocity Float Float

    | UpdateFps


updateFps : Model -> Model
updateFps model =
    { model | nextFps = model.nextFps + 1 }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TouchMsg touchMsg ->
            Touch.update
                touchMsg
                model.touchModel
                ( \newTouchModel -> { model | touchModel = newTouchModel } )

        MovedTwoFingers x y ->
            ( { model | x = model.x + x, y = model.y + y } |> updateFps
            , Cmd.none
            )

        Pinched amount ->
            ( { model | pinch = model.pinch + amount |> clamp 0 (1/0) } |> updateFps
            , Cmd.none
            )

        Rotated amount ->
            ( { model | radians = model.radians + amount } |> updateFps
            , Cmd.none
            )

        GotFinalVelocity x y ->
            ( { model | vx = x, vy = y }
            , Cmd.none
            )

        UpdateFps ->
            ( { model | fps = model.nextFps, nextFps = 0 }
            , Cmd.none
            )



-- View


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text "Move 2 fingers in this box to adjust the coordinates" ]

        , Touch.element
            [ style "border" "1px solid #444444"
            , style "width" "400px"
            , style "height" "400px"
            ] TouchMsg

        , p [] [ text <| "x: " ++ String.fromFloat model.x ]
        , p [] [ text <| "y: " ++ String.fromFloat model.y ]
        , p [] [ text <| "pinch distance: " ++ String.fromFloat model.pinch ]
        , p [] [ text <| "radians: " ++ String.fromFloat model.radians ]
        , p [] [ text <| "updates per second: " ++ String.fromInt model.fps ]
        , p [] [ text <| "velocity x: " ++ String.fromFloat model.vx ]
        , p [] [ text <| "velocity y: " ++ String.fromFloat model.vy ]

        , div
            [ style "background" "#0088ff"
            , style "position" "relative"
            , style "width" "50px"
            , style "height" "50px"

            , style "left" <|
                String.fromFloat model.x ++ "px"
            , style "top" <|
                String.fromFloat model.y ++ "px"
            , style "transform" <|
                let
                    scale = String.fromFloat <| 1 + model.pinch/50
                in
                "rotate(" ++ String.fromFloat model.radians ++ "rad) "
                ++ "scale(" ++ scale ++ "," ++ scale ++ ")"
            ] []
        ]
