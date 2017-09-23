module Pages.LoadCharts exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.IoData exposing (Io, getIo)
import Json.MemoryData exposing (Memory, getMemory)
import Json.SchedulerData exposing (Scheduler, ShedulersUtilization, getSchedulers, shedulersUtilization)
import Material.Card as Card
import Material.Elevation as Elevation
import Material.Grid exposing (..)
import Material.Options exposing (css)
import Time exposing (Time, second)
import Views.Page


type alias Model =
    { shedulers_graph_data :
        List
            { time : String
            , shedulers_utilization : List ShedulersUtilization
            }
    , last_schedulers :
        Dict Int
            { activetime : Int
            , totaltime : Int
            }
    , memory_graph_data :
        List
            { time : String
            , memory : List Int
            }
    , io_graph_data :
        List
            { time : String
            , io : List Int
            }
    , last_io : Maybe ( Int, Int )
    }


model : Model
model =
    { shedulers_graph_data = []
    , last_schedulers = Dict.empty
    , memory_graph_data = []
    , io_graph_data = []
    , last_io = Nothing
    }


updateSchedulers : Scheduler -> Model -> Model
updateSchedulers schedulers model =
    let
        l =
            schedulers.scheduler.data
                |> List.map
                    (\scheduler ->
                        ( scheduler.schedulerid
                        , { activetime = scheduler.activetime
                          , totaltime = scheduler.totaltime
                          }
                        )
                    )

        d =
            schedulers.scheduler.data
                |> List.map
                    (\scheduler ->
                        shedulersUtilization scheduler.schedulerid ( scheduler.activetime, scheduler.totaltime ) model.last_schedulers
                    )

        s =
            model.shedulers_graph_data ++ [ { time = schedulers.scheduler.time, shedulers_utilization = d } ]

        m =
            if List.length s > 20 then
                List.drop 1 s
            else
                s
    in
    { model | shedulers_graph_data = m, last_schedulers = Dict.fromList l }


updateMemory : Memory -> Model -> Model
updateMemory memory model =
    let
        s =
            model.memory_graph_data
                ++ [ { time = memory.time
                     , memory =
                        [ memory.total
                        , memory.processes
                        , memory.atom
                        , memory.binary
                        , memory.code
                        , memory.ets
                        ]
                     }
                   ]

        m =
            if List.length s > 20 then
                List.drop 1 s
            else
                s
    in
    { model | memory_graph_data = m }


updateIo : Io -> Model -> Model
updateIo io model =
    let
        s =
            case model.last_io of
                Just ( i, o ) ->
                    model.io_graph_data
                        ++ [ { time = io.time
                             , io =
                                [ io.input - i
                                , io.output - o
                                ]
                             }
                           ]

                Nothing ->
                    []

        m =
            if List.length s > 20 then
                List.drop 1 s
            else
                s
    in
    { model | io_graph_data = m, last_io = Just ( io.input, io.output ) }


fetchdata : Cmd Msg
fetchdata =
    Cmd.batch
        [ Http.send NewSchedulers getSchedulers
        , Http.send NewMemory getMemory
        , Http.send NewIo getIo
        ]


type Msg
    = NewSchedulers (Result Http.Error Scheduler)
    | NewMemory (Result Http.Error Memory)
    | NewIo (Result Http.Error Io)
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        NewSchedulers (Ok scheduler) ->
            ( updateSchedulers scheduler model, Cmd.none )

        NewSchedulers (Err _) ->
            ( model, Cmd.none )

        NewMemory (Ok memory) ->
            ( updateMemory memory model, Cmd.none )

        NewMemory (Err _) ->
            ( model, Cmd.none )

        NewIo (Ok io) ->
            ( updateIo io model, Cmd.none )

        NewIo (Err _) ->
            ( model, Cmd.none )

        Tick _ ->
            ( model, fetchdata )


googleChart : List (Attribute a) -> List (Html a) -> Html a
googleChart =
    Html.node "google-chart"


type DataValue
    = IntWrapper Int
    | StringWrapper String


columns : List String -> List String
columns l =
    "time" :: l


graph_data : List String -> List (List DataValue) -> String
graph_data columns data_list =
    let
        r =
            data_list
                |> List.map
                    (\d ->
                        let
                            l =
                                d
                                    |> List.map
                                        (\item ->
                                            case item of
                                                IntWrapper i ->
                                                    toString i

                                                StringWrapper s ->
                                                    toString s
                                        )
                        in
                        "[" ++ String.join "," l ++ "]"
                    )
    in
    "[" ++ toString columns ++ "," ++ String.join "," r ++ "]"


schedulers_graph : List (Attribute msg) -> Model -> List (Attribute msg)
schedulers_graph atr model =
    let
        cols =
            columns (Dict.keys model.last_schedulers |> List.map toString)

        data =
            model.shedulers_graph_data |> List.map (\graph_data -> { time = graph_data.time, data = graph_data.shedulers_utilization |> List.map (\item -> IntWrapper item.utilization) })

        data_with_time =
            data |> List.map (\item -> StringWrapper (String.slice 11 19 item.time) :: item.data)

        data_string =
            graph_data cols data_with_time
    in
    if List.isEmpty data then
        []
    else
        atr ++ [ attribute "data" data_string ]


memory_graph : Int -> List (Attribute msg) -> Model -> List (Attribute msg)
memory_graph memory_max_value atr model =
    let
        cols =
            columns [ "total", "processes", "atom", "binary", "code", "ets" ]

        data =
            model.memory_graph_data |> List.map (\graph_data -> { time = graph_data.time, data = graph_data.memory |> List.map (\item -> IntWrapper (scaleValue memory_max_value item)) })

        data_with_time =
            data |> List.map (\item -> StringWrapper (String.slice 11 19 item.time) :: item.data)

        data_string =
            graph_data cols data_with_time
    in
    if List.isEmpty data then
        []
    else
        atr ++ [ attribute "data" data_string ]


io_graph : Int -> List (Attribute msg) -> Model -> List (Attribute msg)
io_graph io_max_value atr model =
    let
        cols =
            columns [ "input", "output" ]

        data =
            model.io_graph_data |> List.map (\graph_data -> { time = graph_data.time, data = graph_data.io |> List.map (\item -> IntWrapper (scaleValue io_max_value item)) })

        data_with_time =
            data |> List.map (\item -> StringWrapper (String.slice 11 19 item.time) :: item.data)

        data_string =
            graph_data cols data_with_time
    in
    if List.isEmpty data then
        []
    else
        atr ++ [ attribute "data" data_string ]


scaleValue : Int -> Int -> Int
scaleValue max value =
    let
        kb =
            max // 1024

        mb =
            kb // 1024

        gb =
            mb // 1024
    in
    if gb > 10 then
        value // (1024 * 1024 * 1024)
    else if mb > 10 then
        value // (1024 * 1024)
    else if kb > 0 then
        value // 1024
    else
        value


unit : Int -> String
unit max =
    let
        kb =
            max // 1024

        mb =
            kb // 1024

        gb =
            mb // 1024
    in
    if gb > 10 then
        "GB"
    else if mb > 10 then
        "MB"
    else if kb > 0 then
        "KB"
    else
        "B"


view : Model -> Html Msg
view model =
    let
        memory_max_value =
            model.memory_graph_data
                |> List.foldl
                    (\item acc ->
                        case List.maximum item.memory of
                            Just v ->
                                Basics.max acc v

                            Nothing ->
                                acc
                    )
                    0

        io_max_value =
            model.io_graph_data
                |> List.foldl
                    (\item acc ->
                        case List.maximum item.io of
                            Just v ->
                                Basics.max acc v

                            Nothing ->
                                acc
                    )
                    0

        attr =
            [ attribute "type" "line"
            , attribute "options" "{\"chartArea\": {\"height\": \"100%\"}, \"vAxis\": {\"textPosition\": \"in\"}, \"hAxis\": {\"maxAlternation\":1, \"textPosition\": \"in\"}}"
            , attribute "style" "width:90%; margin:auto"
            ]
    in
    [ cell [ Material.Grid.size All 12 ]
        [ Card.view [ Elevation.e2, css "margin" "auto", css "width" "100%" ]
            [ Card.title [] [ Card.head [] [ text "Scheduler Utilization(%)" ] ]
            , Card.text [] [ googleChart (schedulers_graph attr model) [] ]
            ]
        ]
    , cell [ Material.Grid.size Desktop 6, Material.Grid.size All 12 ]
        [ Card.view [ Elevation.e2, css "margin" "auto", css "width" "100%" ]
            [ Card.title [] [ Card.head [] [ text ("Memory Usage (" ++ unit memory_max_value ++ ")") ] ]
            , Card.text [] [ googleChart (memory_graph memory_max_value attr model) [] ]
            ]
        ]
    , cell [ Material.Grid.size Desktop 6, Material.Grid.size All 12 ]
        [ Card.view [ Elevation.e2, css "margin" "auto", css "width" "100%" ]
            [ Card.title [] [ Card.head [] [ text ("Io Usage (" ++ unit io_max_value ++ ")") ] ]
            , Card.text [] [ googleChart (io_graph io_max_value attr model) [] ]
            ]
        ]
    ]
        |> grid []
        |> Views.Page.body


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every second Tick
