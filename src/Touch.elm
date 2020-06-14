module Touch exposing
    ( element
    , Msg
    , Model
    , initModel
    , update

    , onMove
    )

import Html exposing (Html)
import Html.Attributes as Attr

import Touch.Internal as Internal


type Msg
    = Msg Internal.Msg


type Model msg
    = Model ( Internal.Model msg )


type alias Listener msg =
    Internal.Listener msg


initModel : List ( Listener msg ) -> Model msg
initModel =
    Internal.initModel >> Model


update : Msg -> Model msg -> ( Model msg -> model ) -> ( model, Cmd msg )
update ( Msg msg ) ( Model model ) updater =
    Internal.update msg model (Model >> updater)


element : List ( Html.Attribute msg ) -> ( Msg -> msg ) -> Html msg
element customAttrs msgWrapper =
    let
        --attrs : List (Html.Attribute msg)
        attrs =
            Internal.attrs
                |> List.map ( Attr.map (Msg >> msgWrapper) )
    in
    Html.div
        ( customAttrs ++ attrs )
        []


onMove : { fingers : Int } -> ( Float -> Float -> msg ) -> Listener msg
onMove =
    Internal.OnMove
