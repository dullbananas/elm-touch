module Touch.Internal exposing (..)

import Html.Events.Extra.Touch as T
import Dict exposing (Dict)
import Html
import Task exposing (Task)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)


attrs : List ( Html.Attribute Msg )
attrs =
    [ T.onStart Event
    , T.onMove Event
    , T.onEnd Event
    , T.onCancel Event
    ]


type alias Model msg =
    { currentTouches : Dict Int Vec2
    , previousTouches : Dict Int Vec2
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
                                case clientPos of
                                    ( clientX, clientY ) ->
                                        Tuple.pair
                                            identifier
                                            ( vec2 clientX clientY )
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
        touchPositions : List { previous : Vec2, current : Vec2 }
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
                touchDeltas : List Vec2
                touchDeltas =
                    List.map
                        ( \{ previous, current } ->
                            {-{ x = current.x - previous.x
                            , y = current.y - previous.y
                            }-}
                            Vec2.sub current previous
                        ) touchPositions

                averageDelta : { x : Float, y : Float }
                averageDelta =
                    { x = average <| List.map Vec2.getX touchDeltas
                    , y = average <| List.map Vec2.getY touchDeltas
                    }
            in
            if fingers == List.length touchDeltas
                then Just <| createMsg
                    ( averageDelta.x )
                    ( averageDelta.y )
                else Nothing

        OnPinch createMsg ->
            case touchPositions of
                [ point1, point2 ] ->
                    let
                        previousDistance : Float
                        previousDistance =
                            Vec2.distance point1.previous point2.previous

                        currentDistance : Float
                        currentDistance =
                            Vec2.distance point1.current point2.current

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


{-calcDistance : Point -> Point -> Float
calcDistance a b =
    Vec2.distance
        ( Vec2.fromRecord a )
        ( Vec2.fromRecord b )-}
    -- https://github.com/justgook/alt-linear-algebra/blob/2.0.0/src/AltMath/Vector2.elm#L117
    {-let
        x = a.x - b.x
        y = a.y - b.y
    in
    sqrt (x*x + y*y)-}


calcAngle : Vec2 -> Vec2 -> Maybe Float
calcAngle a b =
    -- from elm-geometry
    let
        v =
            {-{ x = b.x - a.x
            , y = b.y - a.y
            }-}
            Vec2.sub b a
                --|> Vec2.toRecord

        largestComponent =
            --max (abs v.x) (abs v.y)
            max
                ( abs <| Vec2.getX v )
                ( abs <| Vec2.getY v )
    in
    if largestComponent == 0
        then
            Nothing

        else
            let
            {-scaledX =
                v.x / largestComponent

            scaledY =
                v.y / largestComponent

            scaledLength =
                sqrt (scaledX * scaledX + scaledY * scaledY)-}
                direction =
                    Vec2.normalize v

                angle =
                    atan2
                        ( Vec2.getY direction )
                        ( Vec2.getX direction )
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
