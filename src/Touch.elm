module Touch exposing
    ( element
    , Msg
    , Model
    , initModel
    , updateModel
    )

import Html exposing (Html)
import Html.Attributes as Attr

import Touch.Internal as Internal


type alias Msg =
    Internal.Msg


type alias Model =
    Internal.Model


initModel : Model
initModel =
    Internal.initModel


updateModel : Msg -> Model -> Model
updateModel =
    Internal.updateModel


element : List ( Html.Attribute msg ) -> ( Msg -> msg ) -> Html msg
element customAttrs msgWrapper =
    let
        --attrs : List (Html.Attribute msg)
        attrs =
            Internal.attrs
                |> List.map ( Attr.map msgWrapper )
    in
    Html.div
        ( customAttrs ++ attrs )
        []
