module Main exposing (..)

import Browser
import Calendar exposing (Msg(..), View(..))
import Html exposing (Html, div, text, form, input, select, option, span, label)
import Html.Attributes exposing (type_, value, class, placeholder)
import Html.Events exposing (onSubmit, onInput, onClick)
import Date exposing (Date)
import String exposing (toInt)
import Task
import Time exposing (utc)
import Time.Extra exposing (Parts, partsToPosix)
import List exposing (range, map)
import String exposing (fromInt, fromFloat, pad)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \model -> Sub.none
        }


type alias Model =
    { calendar : Calendar.Model
    , pass :
        { hour : Maybe Int
        , minute : Maybe Int
        }
    }


type Msg
    = CalendarMessage Calendar.Msg
    | CreateReminder Date
    | SetHour String
    | SetMinute String


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( calendar, subcmd ) =
            Calendar.init ()
    in
        ( { calendar = calendar
          , pass =
                { hour = Nothing
                , minute = Nothing
                }
          }
        , (Cmd.map CalendarMessage subcmd)
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateReminder date ->
            case ( model.pass.hour, model.pass.minute ) of
                ( Just hour, Just minute ) ->
                    (let
                        event =
                            Calendar.Reminder date
                                (partsToPosix utc (Parts (Date.year date) (Date.month date) (Date.day date) hour minute 0 0))
                                { id = "my-id", brief = "Some event" }

                        ( calendar, subcmd ) =
                            Calendar.update (Calendar.AddEvent event) model.calendar
                     in
                        ( { model | calendar = calendar }, (Cmd.map CalendarMessage subcmd) )
                    )

                ( _, _ ) ->
                    ( model, Cmd.none )

        SetHour hour ->
            let
                pass =
                    model.pass
            in
                ( { model | pass = { pass | hour = toInt hour } }, Cmd.none )

        SetMinute minute ->
            let
                pass =
                    model.pass
            in
                ( { model | pass = { pass | minute = toInt minute } }, Cmd.none )

        CalendarMessage submsg ->
            let
                ( calendar, subcmd ) =
                    Calendar.update submsg model.calendar
            in
                ( { model | calendar = calendar }, (Cmd.map CalendarMessage subcmd) )


view : Model -> Html Msg
view model =
    div []
        [ select []
            [ option [ onClick (CalendarMessage (SetView YearView)) ] [ text "Year" ]
            , option [ onClick (CalendarMessage (SetView MonthView)) ] [ text "Month" ]
            , option [ onClick (CalendarMessage (SetView WeekView)) ] [ text "Week" ]
            , option [ onClick (CalendarMessage (SetView DayView)) ] [ text "Day" ]
            ]
        , (case model.calendar.selected of
            Just selected ->
                eventcreateview selected model

            Nothing ->
                div
                    []
                    []
          )
        , Html.map CalendarMessage (Calendar.view model.calendar)
        ]


timeselector : List (Html.Attribute Msg) -> Int -> Html Msg
timeselector attributes granularity =
    let
        intervals =
            range 0 (24 * (round (60 / (toFloat granularity))))
    in
        select attributes
            (map
                (\h ->
                    (let
                        hour =
                            floor ((toFloat h) / (60 / (toFloat granularity)))

                        minute =
                            (modBy 60 (h * granularity))
                     in
                        option []
                            [ text (pad 2 '0' (fromInt hour))
                            , text ":"
                            , text (pad 2 '0' (fromInt minute))
                            ]
                    )
                )
                intervals
            )


eventcreateview : Date -> Model -> Html Msg
eventcreateview selected model =
    let
        granularity =
            30
    in
        div [ class "dialog-overlay" ]
            [ div [ class "dialog" ]
                [ form [ onSubmit (CreateReminder selected) ]
                    [ div [ class "dialog-title" ] [ text "Skapa pass" ]
                    , input [ type_ "text", class "event-creator-title", placeholder "Lägg till titel..." ] []
                    , div [ class "form-field-group" ]
                        [ span [ class "event-creator-date" ] [ text (Date.format "MMM d" selected) ]
                        , timeselector [ class "event-creator-time" ] granularity
                        ]
                    , div [ class "dialog-actions" ]
                        [ input [ type_ "reset", value "Stäng" ] []
                        , input [ type_ "submit", value "Skapa" ] []
                        ]
                    ]
                ]
            ]
