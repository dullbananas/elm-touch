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
    case listener of
        OnMove { fingers } createMsg ->
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


average : List Float -> Float
average nums =
    List.sum nums / toFloat ( List.length nums )


triggerMsgs : List msg -> Cmd msg
triggerMsgs =
    List.map
        ( \msg -> Task.succeed msg |> Task.attempt (always msg) )
        >> Cmd.batch
