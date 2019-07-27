module Main exposing (Model, Msg(..), main)

import Browser exposing (sandbox)
import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, string)
import String exposing (startsWith)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { radioState : RadioState
    , alarmState : AlarmState
    }


type RadioState
    = Playing
    | NotPlaying
    | RadioLoading
    | RadioFailure


type AlarmState
    = On
    | Off
    | AlarmLoading
    | AlarmFailure


init : () -> ( Model, Cmd Msg )
init _ =
    ( { radioState = RadioLoading, alarmState = AlarmLoading }
    , Cmd.batch
        [ Http.get
            { url = "http://192.168.0.234/radio/status"
            , expect = Http.expectJson GotJson jsonDecoder
            }
        , Http.get
            { url = "http://192.168.0.234/alarm/status"
            , expect = Http.expectJson GotJson jsonDecoder
            }
        ]
    )


sendStartRadio : Cmd Msg
sendStartRadio =
    Http.get
        { url = "http://192.168.0.234/radio/start"
        , expect = Http.expectJson GotJson jsonDecoder
        }


sendStopRadio : Cmd Msg
sendStopRadio =
    Http.get
        { url = "http://192.168.0.234/radio/stop"
        , expect = Http.expectJson GotJson jsonDecoder
        }


sendAlarmOn : Cmd Msg
sendAlarmOn =
    Http.get
        { url = "http://192.168.0.234/alarm/on"
        , expect = Http.expectJson GotJson jsonDecoder
        }


sendAlarmOff : Cmd Msg
sendAlarmOff =
    Http.get
        { url = "http://192.168.0.234/alarm/off"
        , expect = Http.expectJson GotJson jsonDecoder
        }



-- UPDATE


type Msg
    = GotJson (Result Http.Error String)
    | StartRadio
    | StopRadio
    | SetAlarmOn
    | SetAlarmOff


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotJson result ->
            case result of
                Ok fullText ->
                    if fullText == "stopped" then
                        ( { model | radioState = NotPlaying }, Cmd.none )

                    else if fullText == "started" then
                        ( { model | radioState = Playing }, Cmd.none )

                    else if fullText == "ok let's start this" then
                        ( { model | radioState = Playing }, Cmd.none )

                    else if fullText == "ok let's stop this" then
                        ( { model | radioState = NotPlaying }, Cmd.none )

                    else if fullText == "off" then
                        ( { model | alarmState = Off }, Cmd.none )

                    else if String.startsWith "on at" fullText then
                        ( { model | alarmState = On }, Cmd.none )

                    else if fullText == "ok, set alarm on" then
                        ( { model | alarmState = On }, Cmd.none )

                    else if fullText == "ok, set alarm off" then
                        ( { model | alarmState = Off }, Cmd.none )

                    else
                        ( { model | radioState = RadioFailure }, Cmd.none )

                Err _ ->
                    ( { model | radioState = RadioFailure }, Cmd.none )

        StartRadio ->
            ( { model | radioState = RadioLoading }, sendStartRadio )

        StopRadio ->
            ( { model | radioState = RadioLoading }, sendStopRadio )

        SetAlarmOn ->
            ( { model | alarmState = AlarmLoading }, sendAlarmOn )

        SetAlarmOff ->
            ( { model | alarmState = AlarmLoading }, sendAlarmOff )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "RPi Radio Alarm" ]
        , case model.radioState of
            RadioFailure ->
                text "A failure happened."

            RadioLoading ->
                button [] [ text "Loading" ]

            Playing ->
                button [ onClick StopRadio ] [ text "Radio playing" ]

            NotPlaying ->
                button [ onClick StartRadio ] [ text "Radio not playing" ]
        , case model.alarmState of
            AlarmFailure ->
                text "A failure happened."

            AlarmLoading ->
                button [] [ text "Loading" ]

            Off ->
                button [ onClick SetAlarmOn ] [ text "Alarm off" ]

            On ->
                button [ onClick SetAlarmOff ] [ text "Alarm On" ]
        ]



-- HTTP


jsonDecoder : Decoder String
jsonDecoder =
    field "status" string
