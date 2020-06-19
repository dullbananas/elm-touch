module Touch exposing
    ( element
    , Msg
    , Model
    , initModel
    , initCmd
    , update

    , Listener
    , onMove
    , onPinch
    , onRotate

    , Getter
    , getEndVelocity
    )

{-| This module exposes an API for handling touch events.

# Setup
@docs Msg, Model, initModel, update, element

# Coordinates

Coordinates (returned by some listeners) start at the top left:

    ┏━━━━━━━━━━━━━━━━━▶
    ┃(0,0) (1,0) (2,0)
    ┃
    ┃(0,1) (1,1) (2,1)
    ┃
    ┃(0,2) (1,2) (2,2)
    ┃
    ┃(0,3) (1,3) (2,3)
    ▼

# Listeners
@docs Listener, onMove, onPinch, onRotate

# Getters
@docs Getter, getEndVelocity
-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Lazy
import Touch.Internal as Internal



-- Types


{-| Represents messages handled by this library
-}
type Msg
    = Msg Internal.Msg


{-| Represents the internal model used by this library. The `msg` should be the
type of message used in your library; not `Touch.Msg`.
-}
type Model msg
    = Model ( Internal.Model msg )


{-| Listeners are like subscriptions but for touch events. They are triggered
by finger movement.
-}
type Listener msg
    = Listener ( Internal.ListenerConfig msg )


{-| This is also like a subscription but is triggerd at the start or end of a
touch event.
-}
type Getter msg
    = Getter ( Internal.Getter msg )



-- Setup


{-| Use this to initialize the state of `Touch.Model` and to specify the
listeners.
-}
initModel : List ( Listener msg ) -> List ( Getter msg ) -> Model msg
initModel listeners getters =
    Model <| Internal.initModel
        ( List.map (\(Listener a) -> a) listeners )
        ( List.map (\(Getter a) -> a) getters )
    --List.map (\(Listener a) -> a) >> Internal.initModel >> Model


{-| An initial `Cmd msg` that has to be run. Use it like this:
-}
initCmd : ( Msg -> msg ) -> Cmd msg
initCmd tag =
    Internal.initCmd
        |> Cmd.map ( Msg >> tag )


{-| This is used to update `Touch.Model` in response to a `Touch.Msg`. It also
creates a `Cmd` to trigger listeners. Use it like this:

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            TouchMsg touchMsg ->
                Touch.update
                    touchMsg
                    model.touchMsg
                    ( \newTouchModel -> { model | touchModel = newTouchModel } )

-}
update : Msg -> Model msg -> ( Model msg -> model ) -> ( model, Cmd msg )
update ( Msg msg ) ( Model model ) updater =
    Internal.update msg model (Model >> updater)


{-| Creates an HTML element that responds to touch events. The second argument
must be a function that converts a `Touch.Msg` to `msg`.
-}
element : List ( Html.Attribute msg ) -> ( Msg -> msg ) -> Html msg
element =
    Html.Lazy.lazy2 el


el : List ( Html.Attribute msg ) -> ( Msg -> msg ) -> Html msg
el customAttrs msgWrapper =
    let
--      attrs : List (Html.Attribute msg)
        attrs =
            Internal.attrs
                |> List.map ( Attr.map (Msg >> msgWrapper) )
    in
    Html.div
        ( customAttrs ++ attrs )
        []



-- Listeners


{-| A listener that triggers when fingers move. The second argument must take the
X and Y distances (in pixels) and return a `msg`. The X and Y values are deltas,
so they are higher when the fingers are moving faster and 0 when they don't move.
-}
onMove : { fingers : Int } -> ( Float -> Float -> msg ) -> Listener msg
onMove a =
    initListener << Internal.OnMove a


{-| Triggers when two fingers move apart. A negative value occurs when the
fingers come closer together.
-}
onPinch : ( Float -> msg ) -> Listener msg
onPinch =
    initListener << Internal.OnPinch


{-| Triggered when two fingers rotate. This uses standard Elm angles (radians).
One full turn is `pi * 2`.
-}
onRotate : ( Float -> msg ) -> Listener msg
onRotate =
    initListener << Internal.OnRotate


initListener : Internal.Listener msg -> Listener msg
initListener listener =
    Listener
        { listener = listener
        }



-- Getters


{-| Gets the final velocity vector of the touch movement.
-}
getEndVelocity : ( Float -> Float -> msg ) -> Getter msg
getEndVelocity =
    initGetter << Internal.GetEndVelocity


initGetter : Internal.Getter msg -> Getter msg
initGetter =
    Getter
