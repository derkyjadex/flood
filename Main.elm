module Main exposing (..)

import Array exposing (..)
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Time exposing (..)
import Debug


type alias Model =
    { towers : Array Float
    , buckets : Array Float
    }


type Msg
    = Rain Int
    | Tick


init : ( Model, Cmd Msg )
init =
    ( { towers = Array.fromList [ 8, 5, 2, 7, 3, 1, 8, 6, 5, 9 ]
      , buckets = Array.fromList [ 0, 5, 5, 0, 5, 5, 5, 5, 0, 0 ]
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( flow model, Cmd.none )

        Rain i ->
            ( fillBucket i 0.2 model, Cmd.none )


fillBucket : Int -> Float -> Model -> Model
fillBucket i amount model =
    case Array.get i model.buckets of
        Just h ->
            { model | buckets = Array.set i (h + amount) model.buckets }

        Nothing ->
            model


flow : Model -> Model
flow model =
    List.foldl flowBucket
        model
        (List.range 0 (Array.length model.buckets))


flowBucket : Int -> Model -> Model
flowBucket i model =
    let
        ( lt, lb ) =
            getHeights (i - 1) model

        ( t, b ) =
            getHeights i model

        ( rt, rb ) =
            getHeights (i + 1) model

        lAmount =
            if t + b > lt + lb then
                Basics.min 0.05 (b / 3)
            else
                0

        rAmount =
            if t + b > rt + rb then
                Basics.min 0.05 (b / 3)
            else
                0
    in
        if b < 0 then
            model
        else
            fillBucket i -(lAmount + rAmount) model
                |> fillBucket (i - 1) lAmount
                |> fillBucket (i + 1) rAmount


getHeights : Int -> Model -> ( Float, Float )
getHeights i model =
    case ( Array.get i model.towers, Array.get i model.buckets ) of
        ( Just t, Just b ) ->
            ( t, b )

        _ ->
            ( 100000, 0 )


subscriptions : Model -> Sub Msg
subscriptions model =
    every (100 * millisecond) (\_ -> Tick)


view : Model -> Html Msg
view model =
    svg
        [ width "500"
        , height "500"
        ]
        (List.concat
            [ viewSky, viewTowers model, viewBuckets model ]
        )


viewSky : List (Svg Msg)
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
