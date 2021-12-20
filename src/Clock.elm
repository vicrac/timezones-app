module Clock exposing (view)

import Html exposing (Attribute, Html)
import Svg exposing (Svg, circle, line, svg)
import Svg.Attributes exposing (..)
import Svg.Lazy exposing (lazy)
import Time exposing (Posix, Zone)


view : Zone -> Posix -> Html msg
view zone time =
    let
        hour : Int
        hour =
            Time.toHour zone time

        minute : Int
        minute =
            Time.toMinute zone time

        second : Int
        second =
            Time.toSecond zone time
    in
    svg
        [ width "500"
        , height "500"
        , viewBox "-100 -100 200 200"
        ]
        (markers
            ++ [ -- hourHand doesn't really change that often, so maybe `lazy` it?
                 lazy hourHand <| toFloat hour * 30 + toFloat minute / 10
               , minHand <| toFloat (minute * 6) + toFloat second / 10
               , secHand <| toFloat second * 6
               ]
        )


markers : List (Svg msg)
markers =
    centerDot
        :: List.concatMap
            (\deg ->
                [ hourMarker deg
                , minMarker (deg + 6)
                , minMarker (deg + 12)
                , minMarker (deg + 18)
                , minMarker (deg + 24)
                ]
            )
            [ 0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330 ]


centerDot : Svg msg
centerDot =
    circle [ cx "0", cy "0", r "9", fill "black" ] []


hourMarker : Float -> Svg msg
hourMarker deg =
    line
        [ x1 "0"
        , y1 "95"
        , x2 "0"
        , y2 "78"
        , strokeWidth "3.8"
        , stroke "black"
        , rotate deg
        ]
        []


minMarker : Float -> Svg msg
minMarker deg =
    circle
        [ cx "0 "
        , cy "87"
        , r "2.2"
        , fill "black"
        , rotate deg
        ]
        []


hourHand : Float -> Svg msg
hourHand deg =
    line
        [ x1 "0"
        , y1 "-62"
        , x2 "0"
        , y2 "0"
        , strokeWidth "5"
        , stroke "black"
        , rotate deg
        ]
        []


minHand : Float -> Svg msg
minHand deg =
    line
        [ x1 "0"
        , y1 "-90"
        , x2 "0"
        , y2 "0"
        , strokeWidth "2.8"
        , stroke "black"
        , rotate deg
        ]
        []


secHand : Float -> Svg msg
secHand deg =
    line
        [ x1 "0"
        , y1 "-95"
        , x2 "0"
        , y2 "0"
        , strokeWidth "1.4"
        , stroke "black"
        , rotate deg
        ]
        []


rotate : Float -> Attribute msg
rotate deg =
    transform ("rotate(" ++ String.fromFloat deg ++ ")")
