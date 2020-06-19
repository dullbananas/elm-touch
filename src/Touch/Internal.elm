module Touch.Internal exposing (..)

import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Dict exposing (Dict)
import Html
import Task exposing (Task)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Time


pi2 : Float
pi2 =
    pi * 2


attrs : List ( Html.Attribute Msg )
attrs =
    [ eventAttr "touchmove" Moved
    , eventAttr "touchstart" Started
    , eventAttr "touchend" Ended
    ]


eventAttr : String -> ( TouchEvent -> Msg ) -> Html.Attribute Msg
eventAttr name tag =
    Html.Events.custom
        name
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
    , getters : List ( Getter msg )
    , currentMillis : Int
    }


initModel : List ( ListenerConfig msg ) -> List ( Getter msg ) -> Model msg
initModel listeners getters =
    { currentTouches = Dict.empty
    , previousTouches = Dict.empty
    , listeners = listeners
    , getters = getters
    , currentMillis = 0
    , previousMillis = 0
    }


initCmd : Cmd Msg
initCmd =
    Cmd.batch
        [ getTime
        ]


getTime : Cmd Msg
getTime =
    Time.now
        |> Task.map Time.posixToMillis
        |> Task.perform GotTime


type Msg
    = GotTime Int
    | Started TouchEvent
    | Moved TouchEvent
    | Ended TouchEnded


type alias ListenerConfig msg =
    { listener : Listener msg
    }


type Listener msg
    = OnMove { fingers : Int } ( Float -> Float -> msg )
    | OnPinch ( Float -> msg )
    | OnRotate ( Float -> msg )


type Getter msg
    = GetEndVelocity ( Float -> Float -> msg )


update : Msg -> Model msg -> ( Model msg -> model ) -> ( model, Cmd msg )
update msg oldModel updater =
    ( case msg of
        GotTime millis ->
            ( { oldModel
              | currentMillis = millis
              , previousMillis = oldModel.currentMillis
              }
            , Cmd.none
            )

        Moved { touches } ->
            let
--              model : Model msg
                model =
                    { oldModel
                    | previousTouches = oldModel.currentTouches
                    , currentTouches =
                        List.map
                            ( \{ identifier, clientPos } ->
                                ( identifier, clientPos )
                            ) touches
                            |> Dict.fromList
                    }
            in
            {-Tuple.pair
                ( updater model )
                <| triggerMsgs <|
                    List.filterMap ( triggerListener model ) model.listeners-}
            ( model
            , Cmd.batch
                [ List.filterMap (triggerListener model) model.listeners
                    |> triggerMsgs
                , getTime
                ]
            )

        Started _ ->
            ( oldModel
            , Cmd.batch
                [ getTime
                ]
            )
    )
        |> Tuple.mapFirst updater


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
                            Vec2.sub current previous
                        ) touchPositions
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


calcAngle : Vec2 -> Vec2 -> Float
calcAngle a b =
    let
        direction =
            Vec2.direction a b

        angle =
            atan2
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
