module Icons
    exposing
        ( rotateCcw
        , rotateCw
        )

import Html exposing (Html)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)

svgFeatherIcon : String -> List (Svg msg) -> Html msg
svgFeatherIcon className =
    svg
        [ class <| "feather feather-" ++ className
        , fill "none"
        , height "48"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , strokeWidth "2"
        , viewBox "0 0 24 24"
        , width "48"
        ]


rotateCcw : Html msg
rotateCcw =
    svgFeatherIcon "rotate-ccw"
        [ Svg.polyline [ points "1 4 1 10 7 10" ] []
        , Svg.path [ d "M3.51 15a9 9 0 1 0 2.13-9.36L1 10" ] []
        ]


rotateCw : Html msg
rotateCw =
    svgFeatherIcon "rotate-cw"
        [ Svg.polyline [ points "23 4 23 10 17 10" ] []
        , Svg.path [ d "M20.49 15a9 9 0 1 1-2.12-9.36L23 10" ] []
        ]