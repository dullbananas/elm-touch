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
    , listeners : List ( ListenerConfig msg )
    }


initModel : List ( ListenerConfig msg ) -> Model msg
initModel listeners =
    { currentTouches = Dict.empty
    , previousTouches = Dict.empty
    , listeners = listeners
    }


type Msg
    = Event T.Event


type alias ListenerConfig msg =
    { listener : Listener msg
    }


type Listener msg
    = OnMove { fingers : Int } ( Float -> Float -> msg )
    | OnPinch ( Float -> msg )
    | OnRotate ( Float -> msg )


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
                    List.filterMap ( triggerListener model ) model.listeners


-- Decide what message a listener will receive when an event occurs
triggerListener : Model msg -> ListenerConfig msg -> Maybe msg
triggerListener model config =
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
    case config.listener of
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
                then Just <| createMsg averageDelta.x averageDelta.y
                else Nothing

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
                        Just <| createMsg delta

                _ ->
                    Nothing

        OnRotate createMsg ->
            case touchPositions of
                [ point1, point2 ] ->
                    let
                        previousAngle : Maybe Float
                        previousAngle =
                            calcAngle point1.previous point2.previous

                        currentAngle : Maybe Float
                        currentAngle =
                            calcAngle point1.current point2.current

                        maybeDelta : Maybe Float
                        maybeDelta =
                            Maybe.map2 (-) currentAngle previousAngle
                    in
                        case maybeDelta of
                            Just delta ->
                                let
                                    absDelta = abs delta
                                in
                                Just <| createMsg <|
                                    if absDelta < pi
                                        then
                                            delta
                                        else
                                            -- prevent prob caused by angle going from pi*2 stright to 0
                                            pi*2 - absDelta

                            Nothing ->
                                Nothing

                _ ->
                    Nothing


average : List Float -> Float
average nums =
    List.sum nums / toFloat ( List.length nums )


calcDistance : Point -> Point -> Float
calcDistance a b =
    -- https://github.com/justgook/alt-linear-algebra/blob/2.0.0/src/AltMath/Vector2.elm#L117
    let
        x = a.x - b.x
        y = a.y - b.y
    in
    sqrt (x*x + y*y)


calcAngle : Point -> Point -> Maybe Float
calcAngle a b =
    -- from elm-geometry
    let
        v =
            { x = b.x - a.x
            , y = b.y - a.y
            }

        largestComponent =
            max (abs v.x) (abs v.y)
    in
    if largestComponent == 0 then
        Nothing

    else
        let
            scaledX =
                v.x / largestComponent

            scaledY =
                v.y / largestComponent

            scaledLength =
                sqrt (scaledX * scaledX + scaledY * scaledY)

            angle =
                atan2
                    ( scaledY / scaledLength )
                    ( scaledX / scaledLength )
        in
        Just <| if angle >= 0 then
            angle
        else
            angle + pi*2


triggerMsgs : List msg -> Cmd msg
triggerMsgs =
    List.map
        ( \msg -> Task.succeed msg |> Task.attempt (always msg) )
        >> Cmd.batch
