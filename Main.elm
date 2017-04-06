module Main exposing (..)

import Array exposing (..)
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


type alias Model =
    { towers : Array Int
    , buckets : Array Float
    }


type Msg
    = Rain Int


init : ( Model, Cmd Msg )
init =
    ( { towers = Array.fromList [ 8, 5, 2, 7, 3, 1, 8, 6, 5, 9 ]
      , buckets = Array.fromList [ 0, 0, 0.2, 0, 0, 1, 0, 0, 0, 0 ]
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rain i ->
            case Array.get i model.buckets of
                Just h ->
                    ( { model | buckets = Array.set i (h + 0.2) model.buckets }
                    , Cmd.none
                    )

                Nothing ->
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


viewTowers : Model -> List (Svg Msg)
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
        (List.range 0 (Array.length model.towers))
        (Array.toList model.towers)
    )


viewBuckets : Model -> List (Svg Msg)
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
        (List.range 0 (Array.length model.towers))
        (Array.toList model.towers)
        (Array.toList model.buckets)
    )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
