module Main exposing (..)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Model =
    { towers : List Int
    }


type Msg
    = Tick


init : ( Model, Cmd Msg )
init =
    ( { towers = [ 8, 5, 2, 7, 3, 1, 8, 6, 5, 9 ]
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
    svg [ width "500", height "500" ]
        (List.concat
            [ viewSky, viewTowers model ]
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
                foo =
                    h * 50
            in
                rect
                    [ width "50"
                    , height (toString foo)
                    , x (toString (i * 50))
                    , y (toString (500 - foo))
                    , fill "grey"
                    ]
                    []
        )
        (List.range 0 (List.length model.towers))
        model.towers
    )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
