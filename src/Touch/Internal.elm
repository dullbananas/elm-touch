module Touch.Internal exposing (..)

import Html.Events.Extra.Touch as T
import Dict exposing (Dict)
import Html


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


type alias Model =
    { currentTouches : Dict Int Point
    , previousTouches : Dict Int Point
    }


initModel : Model
initModel =
    { currentTouches = Dict.empty
    , previousTouches = Dict.empty
    }


type Msg
    = Event T.Event


updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        Event { touches } ->
            { model
            | previousTouches = model.currentTouches
            , currentTouches =
                List.map
                    ( \{ identifier, clientPos } ->
                        Tuple.pair
                            identifier
                            { x = Tuple.first clientPos
                            , y = Tuple.second clientPos }
                    ) touches
                    |> Dict.fromList
            }
