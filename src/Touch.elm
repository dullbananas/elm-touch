module Touch exposing
    ( element
    , Msg
    , Model
    , initModel
    , update

    , Listener
    , onMove
    , onPinch
    )

{-| This module exposes an API for handling touch events.

# Setup
@docs Msg, Model, initModel, update, element

# Listeners
@docs Listener, onMove
-}

import Html exposing (Html)
import Html.Attributes as Attr
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


{-| Listeners are like subscriptions but for touch events.
-}
type alias Listener msg =
    Internal.Listener msg



-- Setup


{-| Use this to initialize the state of `Touch.Model` and to specify the
listeners.
-}
initModel : List ( Listener msg ) -> Model msg
initModel =
    Internal.initModel >> Model


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
element customAttrs msgWrapper =
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
onMove =
    Internal.OnMove


{-| Triggers when two fingers move apart. A negative value occurs when the
fingers come closer together.
-}
onPinch : ( Float -> msg ) -> Listener msg
onPinch =
    Internal.OnPinch
