module Main exposing (..)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


type alias Model =
    { towers : List Int
    , buckets : List Int
    }


type Msg
    = Rain Int


init : ( Model, Cmd Msg )
init =
    ( { towers = [ 8, 5, 2, 7, 3, 1, 8, 6, 5, 9 ]
      , buckets = [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    svg
        [ width "500"
        , height "500"
        ]
        (List.concat
            [ viewSky, viewTowers model, viewBuckets model ]
        )


viewSky =
    [ rect
        [ width "500"
        , height "500"
        , x "0"
        , y "0"
        , fill "orange"
        ]
        []
    ]


viewTowers model =
    (List.map2
        (\i h ->
            let
                top =
                    h * 50
            in
                rect
                    [ width "50"
                    , height (toString top)
                    , x (toString (i * 50))
                    , y (toString (500 - top))
                    , fill "grey"
                    , onClick (Rain i)
                    ]
                    []
        )
        (List.range 0 (List.length model.towers))
        model.towers
    )


viewBuckets model =
    (List.map3
        (\i a b ->
            let
                bottom =
                    a * 50
            in
                rect
                    [ width "50"
                    , height (toString (b * 50))
                    , x (toString (i * 50))
                    , y (toString (500 - bottom - (b * 50)))
                    , fill "lightBlue"
                    ]
                    []
        )
        (List.range 0 (List.length model.towers))
        model.towers
        model.buckets
    )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
