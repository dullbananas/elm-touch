module Touch.Internal exposing (..)

import Html.Events.Extra.Touch as T
import Dict exposing (Dict)
import Html
import Task exposing (Task)


attrs : List ( Html.Attribute Msg )
attrs =
    [ T.onStart Event
    , T.onMove Event
    , T.onEnd Event
    , T.onCancel Event
    ]


type alias Point =
    { x : Float
    , y : Float
    }


type alias Model msg =
    { currentTouches : Dict Int Point
    , previousTouches : Dict Int Point
    , listeners : List ( Listener msg )
    }


initModel : List ( Listener msg ) -> Model msg
initModel listeners =
    { currentTouches = Dict.empty
    , previousTouches = Dict.empty
    , listeners = listeners
    }


type Msg
    = Event T.Event


type Listener msg
    = OnMove { fingers : Int } ( Float -> Float -> msg )
    | OnPinch ( Float -> msg )


update : Msg -> Model msg -> ( Model msg -> model ) -> ( model, Cmd msg )
update msg oldModel updater =
    case msg of
        Event { touches } ->
            let
--              model : Model msg
                model =
                    { oldModel
                    | previousTouches = oldModel.currentTouches
                    , currentTouches =
                        List.map
                            ( \{ identifier, clientPos } ->
                                Tuple.pair
                                    identifier
                                    { x = Tuple.first clientPos
                                    , y = Tuple.second clientPos
                                    }
                            ) touches
                            |> Dict.fromList
                    }
            in
            Tuple.pair
                ( updater model )
                <| triggerMsgs <|
                    List.map ( triggerListener model ) model.listeners


-- Decide what message a listener will receive when an event occurs
triggerListener : Model msg -> Listener msg -> msg
triggerListener model listener =
    let
        touchPositions : List { previous : Point, current : Point }
        touchPositions =
            Dict.map
                ( \id currentPoint ->
                    case Dict.get id model.previousTouches of
                        Nothing ->
                            Nothing

                        Just previousPoint ->
                            Just
                                { previous = previousPoint
                                , current = currentPoint
                                }
                ) model.currentTouches
                |> Dict.values
                |> List.filterMap identity
    in
    case listener of
        OnMove { fingers } createMsg ->
            let
                touchDeltas : List Point
                touchDeltas =
                    List.map
                        ( \{ previous, current } ->
                            { x = current.x - previous.x
                            , y = current.y - previous.y
                            }
                        ) touchPositions

                averageDelta : Point
                averageDelta =
                    { x = average <| List.map .x touchDeltas
                    , y = average <| List.map .y touchDeltas
                    }
            in
            if fingers == List.length touchDeltas
                then createMsg averageDelta.x averageDelta.y
                else createMsg 0 0

        OnPinch createMsg ->
            case touchPositions of
                [ point1, point2 ] ->
                    let
                        previousDistance : Float
                        previousDistance =
                            calcDistance point1.previous point2.previous

                        currentDistance : Float
                        currentDistance =
                            calcDistance point1.current point2.current

                        delta : Float
                        delta =
                            currentDistance - previousDistance
                    in
                        createMsg delta

                _ ->
                    createMsg 0


average : List Float -> Float
average nums =
    List.sum nums / toFloat ( List.length nums )


calcDistance : Point -> Point -> Float
calcDistance a b =
    -- https://github.com/justgook/alt-linear-algebra/blob/2.0.0/src/AltMath/Vector2.elm#L117
    let
        c =
            { x = a.x - b.x, y = a.y - b.y }
    in
    sqrt (c.x * c.x + c.y * c.y)


triggerMsgs : List msg -> Cmd msg
triggerMsgs =
    List.map
        ( \msg -> Task.succeed msg |> Task.attempt (always msg) )
        >> Cmd.batch
