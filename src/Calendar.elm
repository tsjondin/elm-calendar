module Calendar exposing (..)

import Browser
import Date exposing (Date, Interval(..), Unit(..))
import Html exposing (Html, Attribute, div, text, table, tr, td, span)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List exposing (map, range, filter, concat, append)
import Maybe exposing (Maybe)
import Task
import Time exposing (Posix, utc, toHour, toMinute)
import String exposing (fromInt, pad)
import String.Format as Format


type CalendarEvent
    = Event Date CalendarTimeRange EventTag
    | Reminder Date Posix EventTag


type alias EventTag =
    { id : String
    , brief : String
    }


type CalendarTimeRange
    = TimeRange Posix Posix


type View
    = YearView
    | MonthView
    | WeekView
    | DayView


type alias EventManager =
    List CalendarEvent


type alias Model =
    { date : Date
    , today : Date
    , events : EventManager
    , selected : Maybe Date
    , view : View
    }


type Msg
    = YearForward
    | YearBack
    | MonthForward
    | MonthBack
    | WeekForward
    | WeekBack
    | DayForward
    | DayBack
    | DateSelect Date
    | SetView View
    | AddEvent CalendarEvent
    | RemoveEvent CalendarEvent
    | Initialize Date


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \model -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { date = Date.fromRataDie 0
      , today = Date.fromRataDie 0
      , events = []
      , selected = Nothing
      , view = YearView
      }
    , (Task.perform Initialize Date.today)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Initialize date ->
            ( { date = date
              , today = date
              , selected = Nothing
              , events = model.events
              , view = model.view
              }
            , Cmd.none
            )

        YearForward ->
            ( { model | date = Date.add Years 1 model.date }, Cmd.none )

        YearBack ->
            ( { model | date = Date.add Years -1 model.date }, Cmd.none )

        MonthForward ->
            ( { model | date = Date.add Months 1 model.date }, Cmd.none )

        MonthBack ->
            ( { model | date = Date.add Months -1 model.date }, Cmd.none )

        WeekForward ->
            ( { model | date = Date.add Weeks 1 model.date }, Cmd.none )

        WeekBack ->
            ( { model | date = Date.add Weeks -1 model.date }, Cmd.none )

        DayForward ->
            ( { model | date = Date.add Days 1 model.date }, Cmd.none )

        DayBack ->
            ( { model | date = Date.add Days -1 model.date }, Cmd.none )

        DateSelect date ->
            ( { model | selected = Just date }, Cmd.none )

        SetView calendarview ->
            ( { model | view = calendarview }, Cmd.none )

        AddEvent event ->
            case event of
                Event _ _ _ ->
                    ( { model
                        | events =
                            event :: model.events
                      }
                    , Cmd.none
                    )

                Reminder _ _ _ ->
                    ( { model
                        | events =
                            event :: model.events
                      }
                    , Cmd.none
                    )

        RemoveEvent event ->
            case event of
                Reminder _ _ _ ->
                    ( { model
                        | events =
                            (filter
                                (\e -> (e == event))
                                model.events
                            )
                      }
                    , Cmd.none
                    )

                Event _ _ _ ->
                    ( { model
                        | events =
                            (filter
                                (\e -> (e == event))
                                model.events
                            )
                      }
                    , Cmd.none
                    )


view : Model -> Html Msg
view model =
    div
        [ class "calendar" ]
        [ (case model.view of
            YearView ->
                div []
                    [ div [ class "calendar-title" ]
                        [ span [ class "calendar-selector", onClick YearBack ] [ text "◀" ]
                        , span [ class "calendar-title-text" ] [ text ("Year " ++ (Date.format "y" model.date)) ]
                        , span [ class "calendar-selector", onClick YearForward ] [ text "▶" ]
                        ]
                    , (yearview model model.date)
                    ]

            MonthView ->
                div []
                    [ div [ class "calendar-title" ]
                        [ span [ class "calendar-selector", onClick MonthBack ] [ text "◀" ]
                        , span [ class "calendar-title-text" ] [ text (Date.format "MMMM y" model.date) ]
                        , span [ class "calendar-selector", onClick MonthForward ] [ text "▶" ]
                        ]
                    , monthview model model.date
                    ]

            WeekView ->
                let
                    firstOfWeek =
                        Date.fromWeekDate (Date.year model.date) (Date.weekNumber model.date) Time.Mon
                in
                    div []
                        [ div [ class "calendar-title" ]
                            [ span [ class "calendar-selector", onClick WeekBack ] [ text "◀" ]
                            , span [ class "calendar-title-text" ] [ text ("Week " ++ (Date.format "w" model.date)) ]
                            , span [ class "calendar-selector", onClick WeekForward ] [ text "▶" ]
                            ]
                        , (weekview model firstOfWeek 0)
                        ]

            DayView ->
                div []
                    [ div [ class "calendar-title" ]
                        [ span [ class "calendar-selector", onClick DayBack ] [ text "◀" ]
                        , span [ class "calendar-title-text" ] [ text (Date.format "MMMM dd" model.date) ]
                        , span [ class "calendar-selector", onClick DayForward ] [ text "▶" ]
                        ]
                    , (dateview model model.date)
                    ]
          )
        ]


yearview : Model -> Date -> Html Msg
yearview model date =
    div [ class "calendar-year" ]
        (map
            (\month ->
                div [ class "calendar-year-month" ]
                    [ div [ class "calendar-year-month-title" ] [ (text (Date.format "MMMM" month)) ]
                    , monthview model month
                    ]
            )
            (Date.range Month
                1
                (Date.fromCalendarDate (Date.year date) Time.Jan 1)
                (Date.fromCalendarDate (Date.year date) Time.Dec 31)
            )
        )


monthview : Model -> Date -> Html Msg
monthview model date =
    let
        firstOfMonth =
            Date.fromCalendarDate (Date.year model.date) (Date.month date) 1

        dayOfWeekOffset =
            (Date.weekdayNumber firstOfMonth) - 1

        firstDateOfView =
            firstOfMonth
                |> Date.add Days (-dayOfWeekOffset)
    in
        div [ class "calendar-month" ]
            (map
                (\row ->
                    (weekview model firstDateOfView row)
                )
                (range 0 5)
            )


weekview : Model -> Date -> Int -> Html Msg
weekview model date row =
    let
        weekdate =
            date |> Date.add Weeks row
    in
        div [ class "calendar-week" ]
            ((div [ class "calendar-cell" ]
                [ div [ class "calendar-cell-title" ] [ text (Date.format "w" weekdate) ]
                ]
             )
                :: (map
                        (\col ->
                            let
                                itdate =
                                    weekdate |> Date.add Days col
                            in
                                dateview model itdate
                        )
                        (range 0 6)
                   )
            )


dateview : Model -> Date -> Html Msg
dateview model date =
    let
        selected =
            Maybe.withDefault (Date.fromRataDie 0) model.selected

        events =
            filter
                (\e ->
                    (case e of
                        Event eventdate _ _ ->
                            (eventdate == date)

                        Reminder eventdate _ _ ->
                            (eventdate == date)
                    )
                )
                model.events
    in
        div
            [ (onClick (DateSelect date))
            , class "calendar-cell"
            , (if date == selected then
                class "calendar-cell-selected"
               else if date == model.today then
                class "calendar-cell-current"
               else if Date.month date == Date.month model.date then
                class "calendar-cell-active"
               else
                class "calendar-cell-inactive"
              )
            ]
            (div [ class "calendar-cell-title" ] [ text (Date.format "d" date) ]
                :: (map eventview events)
            )


eventview : CalendarEvent -> Html Msg
eventview event =
    case event of
        Reminder date time tag ->
            div [ class "calendar-event" ]
                [ div [ class "calendar-event-header" ]
                    [ div [ class "calendar-event-time" ]
                        [ text
                            ("{{}}:{{}}"
                                |> Format.value (pad 2 '0' (fromInt (toHour utc time)))
                                |> Format.value (pad 2 '0' (fromInt (toMinute utc time)))
                            )
                        ]
                    , text " - "
                    , div [ class "calendar-event-title" ] [ text tag.brief ]
                    ]
                ]

        Event date range tag ->
            div [ class "calendar-event" ]
                [ div [ class "calendar-event-header" ]
                    [ div [ class "calendar-event-time" ]
                        [ text "Foobar"
                        ]
                    , text " - "
                    , div [ class "calendar-event-title" ] [ text tag.brief ]
                    ]
                ]
