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
      , buckets = Array.fromList [ 0, 0, 0.2, 0, 0, 1, 0, 0, 0, 0 ]
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
    case ( getHeights (i - 1) model, getHeights i model, getHeights (i + 1) model ) of
        ( Just ( lt, lb ), Just ( t, b ), Just ( rt, rb ) ) ->
            if b < 0.2 then
                model
            else
                let
                    lAmount =
                        if t + b > lt + lb then
                            0.1
                        else
                            0

                    rAmount =
                        if t + b > rt + rb then
                            0.1
                        else
                            0
                in
                    fillBucket i -(lAmount + rAmount) model
                        |> fillBucket (i - 1) lAmount
                        |> fillBucket (i + 1) rAmount

        ( Just ( lt, lb ), Just ( t, b ), Nothing ) ->
            if b < 0.2 then
                model
            else
                let
                    lAmount =
                        if t + b > lt + lb then
                            0.1
                        else
                            0
                in
                    fillBucket i -(lAmount) model
                        |> fillBucket (i - 1) lAmount

        ( Nothing, Just ( t, b ), Just ( rt, rb ) ) ->
            if b < 0.2 then
                model
            else
                let
                    rAmount =
                        if t + b > rt + rb then
                            0.1
                        else
                            0
                in
                    fillBucket i -(rAmount) model
                        |> fillBucket (i + 1) rAmount

        _ ->
            model


getHeights : Int -> Model -> Maybe ( Float, Float )
getHeights i model =
    case ( Array.get i model.towers, Array.get i model.buckets ) of
        ( Just t, Just b ) ->
            Just ( t, b )

        _ ->
            Nothing


subscriptions : Model -> Sub Msg
subscriptions model =
    every (200 * millisecond) (\_ -> Tick)


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
