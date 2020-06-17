module Example exposing (main)


import Html exposing (..)
import Html.Attributes exposing (..)
import Browser
import Touch



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = always ( initModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }



-- Model


type alias Model =
    { touchModel : Touch.Model Msg
    , x : Float
    , y : Float
    , pinch : Float
    , radians : Float
    }


initModel : Model
initModel =
    { touchModel =
        Touch.initModel
            [ Touch.onMove { fingers = 2 } MovedTwoFingers
            , Touch.onPinch Pinched
            , Touch.onRotate Rotated
            ]
    , x = 0
    , y = 0
    , pinch = 0
    , radians = 0
    }



-- Update


type Msg
    = TouchMsg Touch.Msg
    | MovedTwoFingers Float Float
    | Pinched Float
    | Rotated Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TouchMsg touchMsg ->
            Touch.update
                touchMsg
                model.touchModel
                ( \newTouchModel -> { model | touchModel = newTouchModel } )

        MovedTwoFingers x y ->
            ( { model | x = model.x + x, y = model.y + y }
            , Cmd.none
            )

        Pinched amount ->
            ( { model | pinch = model.pinch + amount |> clamp 0 (1/0) }
            , Cmd.none
            )

        Rotated amount ->
            ( { model | radians = model.radians + amount }
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
                "rotate(" ++ String.fromFloat model.radians ++ "rad)"
            , style "outline" <|
                String.fromFloat (model.pinch/2) ++ "px solid #0088ff"
            ] []
        ]
