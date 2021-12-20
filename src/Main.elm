module Main exposing (main)

import Browser
import Clock
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Task
import Time
import WorldTimeApi


zoneName : String
zoneName =
    "America/Vancouver"


type alias Model =
    { zone : Maybe (Result Http.Error WorldTimeApi.Data)
    , time : Maybe Time.Posix
    , paused : Bool
    }


type Msg
    = GotTimezoneInfo (Result Http.Error WorldTimeApi.Data)
    | GotTime Time.Posix
    | Tick Time.Posix
    | ClickedPauseButton


init : ( Model, Cmd Msg )
init =
    ( { zone = Nothing, time = Nothing, paused = False }
    , Cmd.batch
        [ WorldTimeApi.get GotTimezoneInfo zoneName
        , Task.perform GotTime Time.now
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTimezoneInfo zone ->
            ( { model | zone = Just zone }, Cmd.none )

        GotTime time ->
            ( { model | time = Just time }, Cmd.none )

        Tick time ->
            ( { model | time = Just time }, Cmd.none )

        ClickedPauseButton ->
            let
                newPaused =
                    not model.paused
            in
            ( { model | paused = newPaused }
            , if not newPaused then
                Task.perform GotTime Time.now

              else
                Cmd.none
            )


view : Model -> Html Msg
view model =
    div [] [ h1 [] [ text "Timezones" ], viewTime model ]


viewTime : Model -> Html Msg
viewTime model =
    case Maybe.map2 Tuple.pair model.zone model.time of
        Nothing ->
            p [] [ text "loading..." ]

        Just ( Ok zone, time ) ->
            div []
                [ p [] [ text <| formatTime zone time ]
                , Clock.view (WorldTimeApi.zone zone) time
                , button [ onClick ClickedPauseButton ]
                    [ text <|
                        if model.paused then
                            "Resume"

                        else
                            "Pause"
                    ]
                ]

        _ ->
            p [] [ text "error" ]


formatTime : WorldTimeApi.Data -> Time.Posix -> String
formatTime data time =
    let
        zone =
            WorldTimeApi.zone data
    in
    WorldTimeApi.name data
        ++ " ("
        ++ WorldTimeApi.abbreviation data
        ++ ")"
        ++ "  - "
        ++ String.padLeft 2 '0' (String.fromInt (Time.toHour zone time))
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt (Time.toMinute zone time))
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt (Time.toSecond zone time))


subscriptions : Model -> Sub Msg
subscriptions { zone, paused } =
    case zone of
        Just (Ok _) ->
            if paused then
                Sub.none

            else
                Time.every 1000 Tick

        _ ->
            Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
