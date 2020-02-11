module Main exposing (Model, Msg(..), main)

import Browser exposing (sandbox)
import Delay
import Html exposing (Html, button, div, h1, input, p, text)
import Html.Attributes exposing (class, max, maxlength, min, placeholder, step, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, string)
import Platform



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
    , alarmTime : AlarmTime
    , alarmTimeStatus : AlarmTimeStatus
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


type AlarmTimeStatus
    = None
    | Saving
    | Saved
    | Failed


type alias AlarmTime =
    { hours : String, mins : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { radioState = RadioLoading
      , alarmState = AlarmLoading
      , alarmTime = { hours = "", mins = "" }
      , alarmTimeStatus = None
      }
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


sendChangeAlarmTime : String -> String -> Cmd Msg
sendChangeAlarmTime hours mins =
    Http.get
        { url = "http://192.168.0.234/alarm/time/" ++ hours ++ ":" ++ mins
        , expect = Http.expectJson GotJson jsonDecoder
        }


removeSavedLater : Cmd Msg
removeSavedLater =
    Delay.after 1 Delay.Second RemoveSaved



-- UPDATE


type Msg
    = GotJson (Result Http.Error String)
    | StartRadio
    | StopRadio
    | SetAlarmOn
    | SetAlarmOff
    | ChangeAlarmTimeHours String
    | ChangeAlarmTimeMins String
    | RemoveSaved


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
                        ( { model
                            | alarmState = On
                            , alarmTime = extractTime fullText
                          }
                        , Cmd.none
                        )

                    else if fullText == "ok, set alarm on" then
                        ( { model | alarmState = On }, Cmd.none )

                    else if fullText == "ok, set alarm off" then
                        ( { model | alarmState = Off }, Cmd.none )

                    else if String.startsWith "time set to" fullText then
                        ( { model | alarmTimeStatus = Saved }, removeSavedLater )

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

        ChangeAlarmTimeMins mins ->
            if isTimesValid model.alarmTime.hours mins then
                ( { model
                    | alarmTimeStatus = Saving
                    , alarmTime =
                        { hours = model.alarmTime.hours, mins = mins }
                  }
                , sendChangeAlarmTime model.alarmTime.hours mins
                )

            else
                ( { model
                    | alarmTimeStatus = None
                    , alarmTime = { hours = model.alarmTime.hours, mins = mins }
                  }
                , Cmd.none
                )

        ChangeAlarmTimeHours hours ->
            if isTimesValid hours model.alarmTime.mins then
                ( { model
                    | alarmTimeStatus = Saving
                    , alarmTime = { hours = hours, mins = model.alarmTime.mins }
                  }
                , sendChangeAlarmTime hours model.alarmTime.mins
                )

            else
                ( { model
                    | alarmTimeStatus = None
                    , alarmTime = { hours = hours, mins = model.alarmTime.mins }
                  }
                , Cmd.none
                )

        RemoveSaved ->
            ( { model
                | alarmTimeStatus = None
              }
            , Cmd.none
            )


isTimesValid : String -> String -> Bool
isTimesValid hours mins =
    case ( String.toInt hours, String.toInt mins ) of
        ( Just h, Just m ) ->
            isValidHours h && isValidMins m

        _ ->
            False


extractTime : String -> AlarmTime
extractTime text =
    case listToTuple2 (String.split ":" (String.slice 6 11 text)) of
        Just ( hours, mins ) ->
            { hours = hours, mins = mins }

        Nothing ->
            { hours = "", mins = "" }


listToTuple2 : List String -> Maybe ( String, String )
listToTuple2 list =
    case list of
        [ a, b ] ->
            Just ( a, b )

        _ ->
            Nothing


extractHours : String -> Maybe Int
extractHours text =
    let
        hours =
            String.toInt text
    in
    case hours of
        Just h ->
            if isValidHours h then
                Just h

            else
                Nothing

        Nothing ->
            Nothing


isValidHours : Int -> Bool
isValidHours hours =
    hours >= 0 && hours < 24


extractMins : String -> Maybe Int
extractMins text =
    let
        mins =
            String.toInt text
    in
    case mins of
        Just m ->
            if isValidMins m then
                Just m

            else
                Nothing

        Nothing ->
            Nothing


isValidMins : Int -> Bool
isValidMins mins =
    mins >= 0 && mins < 60



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
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
                div []
                    [ button [ onClick SetAlarmOff ] [ text "Alarm On" ]
                    , input
                        [ placeholder "Hours"
                        , value model.alarmTime.hours
                        , onInput ChangeAlarmTimeHours
                        , maxlength 2
                        , Html.Attributes.min "0"
                        , step "1"
                        , Html.Attributes.max "23"
                        ]
                        []
                    , div [ class "colon" ] [ text ":" ]
                    , input
                        [ placeholder "Mins"
                        , value model.alarmTime.mins
                        , onInput ChangeAlarmTimeMins
                        , maxlength 2
                        , Html.Attributes.min "0"
                        , step "1"
                        , Html.Attributes.max "59"
                        ]
                        []
                    , case model.alarmTimeStatus of
                        None ->
                            text ""

                        Saved ->
                            text "Saved"

                        Saving ->
                            text "Saving"

                        Failed ->
                            text "Failed"
                    ]
        ]



-- HTTP


jsonDecoder : Decoder String
jsonDecoder =
    field "status" string
