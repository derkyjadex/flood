module Main exposing (..)

import Array exposing (..)
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Time exposing (..)


type alias Model =
    { towers : Array Float
    , buckets : Array Float
    , clouds : Array Bool
    }


type Msg
    = Rain Int
    | Tick


init : ( Model, Cmd Msg )
init =
    ( { towers = Array.fromList [ 8, 5, 2, 7, 3, 1, 8, 6, 5, 9 ]
      , buckets = Array.fromList [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
      , clouds = Array.fromList <| List.repeat 10 False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( (rain >> flow) model
            , Cmd.none
            )

        Rain i ->
            ( toggleCloud i model, Cmd.none )


fillBucket : Int -> Float -> Model -> Model
fillBucket i amount model =
    case Array.get i model.buckets of
        Just h ->
            { model | buckets = Array.set i (h + amount) model.buckets }

        Nothing ->
            model


rain : Model -> Model
rain model =
    List.foldl rainCloud
        model
        (List.range 0 (Array.length model.clouds))


rainCloud : Int -> Model -> Model
rainCloud i model =
    if getCloud i model then
        fillBucket i 0.08 model
    else
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


getCloud : Int -> Model -> Bool
getCloud i model =
    case Array.get i model.clouds of
        Just c ->
            c

        Nothing ->
            False


toggleCloud : Int -> Model -> Model
toggleCloud i model =
    case Array.get i model.clouds of
        Just c ->
            { model | clouds = Array.set i (not c) model.clouds }

        Nothing ->
            model


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
            [ viewSky, viewTowers model, viewBuckets model, viewClouds model ]
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


viewClouds : Model -> List (Svg Msg)
viewClouds model =
    (List.map2
        (\i c ->
            let
                h =
                    if c then
                        20
                    else
                        0

                s =
                    if c then
                        "skyBlue"
                    else
                        "none"
            in
                g []
                    [ ellipse
                        [ rx "50"
                        , ry (toString h)
                        , cx (toString (25 + i * 50))
                        , cy "0"
                        , fill "darkGrey"
                        ]
                        []
                    , line
                        [ x1 (toString (15 + i * 50))
                        , y1 "10"
                        , x2 (toString (25 + i * 50))
                        , y2 "30"
                        , stroke s
                        , strokeWidth "5"
                        ]
                        []
                    , line
                        [ x1 (toString (35 + i * 50))
                        , y1 "10"
                        , x2 (toString (45 + i * 50))
                        , y2 "30"
                        , stroke s
                        , strokeWidth "5"
                        ]
                        []
                    ]
        )
        (List.range 0 (Array.length model.clouds))
        (Array.toList model.clouds)
    )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
