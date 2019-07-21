module Main exposing (Model, Msg(..), main)

import Browser exposing (sandbox)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, string)



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


type Model
    = On
    | Off
    | Loading
    | Failure


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "http://192.168.0.234/radio/status"
        , expect = Http.expectJson GotJson jsonDecoder
        }
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



-- UPDATE


type Msg
    = GotJson (Result Http.Error String)
    | StartRadio
    | StopRadio


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotJson result ->
            case result of
                Ok fullText ->
                    if fullText == "stopped" then
                        ( Off, Cmd.none )

                    else if fullText == "started" then
                        ( On, Cmd.none )

                    else if fullText == "ok let's start this" then
                        ( On, Cmd.none )

                    else if fullText == "ok let's stop this" then
                        ( Off, Cmd.none )

                    else
                        ( Failure, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )

        StartRadio ->
            ( Loading, sendStartRadio )

        StopRadio ->
            ( Loading, sendStopRadio )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "A failure happened."

        Loading ->
            button [] [ text "Loading" ]

        On ->
            button [ onClick StopRadio ] [ text "Stop radio" ]

        Off ->
            button [ onClick StartRadio ] [ text "Start radio" ]



-- HTTP


jsonDecoder : Decoder String
jsonDecoder =
    field "status" string
