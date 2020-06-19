module Touch.Internal exposing (..)

import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Dict exposing (Dict)
import Html
import Task exposing (Task)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)


pi2 : Float
pi2 =
    pi * 2


attrs : List ( Html.Attribute Msg )
attrs =
    [ Html.Events.custom
        "touchmove"
        ( touchEventDecoder
            |> Decode.andThen
                ( \event ->
                    Decode.succeed
                        { message = Moved event
                        , stopPropagation = True
                        , preventDefault = True
                        }
                )
        )
        {-( Decode.value
            |> Decode.andThen
                (\v -> Decode.succeed<| always {message=Moved{touches=[]},stopPropagation=False,preventDefault=False}
                    ( Debug.log "error" <| Decode.decodeValue touchEventDecoder v )
                )
        )-}
    ]


type alias TouchEvent =
    { touches : List Touch
    }


type alias Touch =
    { identifier : Int
    , clientPos : Vec2
    }


touchEventDecoder : Decoder TouchEvent
touchEventDecoder =
    Decode.map TouchEvent
        ( Decode.field "touches" <| touchListDecoder touchDecoder )


touchDecoder : Decoder Touch
touchDecoder =
    Decode.map2 Touch
        ( Decode.field "identifier" Decode.int )
        ( Decode.map2 vec2
            ( Decode.field "clientX" Decode.float )
            ( Decode.field "clientY" Decode.float )
        )


touchListDecoder : Decoder a -> Decoder ( List a )
touchListDecoder itemDecoder =
    Decode.field "length" Decode.int
        |> Decode.andThen
            ( \length ->
                List.range (0) (length - 1)
                    |> List.map
                        ( \i -> Decode.field (String.fromInt i) itemDecoder )
                    |> List.foldr
                        ( Decode.map2 (::) )
                        ( Decode.succeed [] )
            )


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
    = Moved TouchEvent


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
        Moved { touches } ->
            let
--              model : Model msg
                model =
                    { oldModel
                    | previousTouches = oldModel.currentTouches
                    , currentTouches =
                        List.map
                            ( \{ identifier, clientPos } ->
                                {-case clientPos of
                                    ( clientX, clientY ) ->
                                        Tuple.pair
                                            identifier
                                            ( vec2 clientX clientY )-}
                                ( identifier, clientPos )
                            ) touches
                            |> Dict.fromList
                    }
            in
            Tuple.pair
                ( updater model )
                <| triggerMsgs <|
                    List.filterMap ( triggerListener model ) model.listeners


getTouchPositions : Model msg -> List { previous : Vec2, current : Vec2 }
getTouchPositions model =
    Dict.foldr
        ( \key value list ->
            case Dict.get key model.previousTouches of
                Nothing ->
                    list

                Just previous ->
                    { previous = previous
                    , current = value
                    } :: list
        ) [] model.currentTouches


-- Decide what message a listener will receive when an event occurs
triggerListener : Model msg -> ListenerConfig msg -> Maybe msg
triggerListener model config =
    let
        touchPositions : List { previous : Vec2, current : Vec2 }
        touchPositions =
            getTouchPositions model
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
                {-averageDelta : { x : Float, y : Float }
                averageDelta =
                    { x = average <| List.map Vec2.getX touchDeltas
                    , y = average <| List.map Vec2.getY touchDeltas
                    }-}
            in
            if fingers == List.length touchDeltas
                then
                    let
                        averageDelta : { x : Float, y : Float }
                        averageDelta =
                            List.foldr
                                Vec2.add
                                ( vec2 0 0 )
                                touchDeltas
                                |> Vec2.scale ( 1 / toFloat fingers )
                                |> Vec2.toRecord
                    in
                    Just <| createMsg
                        ( averageDelta.x )
                        ( averageDelta.y )

                else
                    Nothing

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
                        previousAngle : Float
                        previousAngle =
                            calcAngle point1.previous point2.previous

                        currentAngle : Float
                        currentAngle =
                            calcAngle point1.current point2.current

                        delta : Float
                        delta =
                            currentAngle - previousAngle

                        absDelta : Float
                        absDelta =
                            abs delta
                    in
                        Just <| createMsg <|
                            if absDelta < pi
                                then
                                    delta
                                else
                                    -- prevent prob caused by angle going from pi*2 stright to 0
                                    pi2 - absDelta

                _ ->
                    Nothing


{-average : List Float -> Float
average nums =
    List.sum nums / toFloat ( List.length nums )-}


calcAngle : Vec2 -> Vec2 -> Float
calcAngle a b =
    -- from elm-geometry
    let
        --v =
            {-{ x = b.x - a.x
            , y = b.y - a.y
            }-}
            --Vec2.sub b a
                --|> Vec2.toRecord

        {-largestComponent =
            --max (abs v.x) (abs v.y)
            max
                ( abs <| Vec2.getX v )
                ( abs <| Vec2.getY v )-}
            {-scaledX =
                v.x / largestComponent

            scaledY =
                v.y / largestComponent

            scaledLength =
                sqrt (scaledX * scaledX + scaledY * scaledY)-}
        direction =
            --Vec2.normalize v
            Vec2.direction a b
                --|> Vec2.toRecord

        angle =
            atan2
                --( direction.y )
                --( direction.x )
                ( Vec2.getY direction )
                ( Vec2.getX direction )
    in
    if angle >= 0 then
        angle
    else
        angle + pi2


triggerMsgs : List msg -> Cmd msg
triggerMsgs =
    List.map
        ( \msg -> Task.succeed ()
            |> Task.attempt (always msg)
        )
        >> Cmd.batch
