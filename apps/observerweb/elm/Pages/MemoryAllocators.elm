module Pages.MemoryAllocators exposing (..)

import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.AllocatorData exposing (Allocator, AllocatorData, getAllocator)
import Material.Card as Card
import Material.Elevation as Elevation
import Material.Grid exposing (..)
import Material.Options exposing (css)
import Material.Table as Table
import Time exposing (Time, second)
import Views.Page


fetchdata : Cmd Msg
fetchdata =
    Cmd.batch
        [ Http.send NewAllocator getAllocator
        ]


type alias CarrierSize =
    { allocator : String
    , carrier_size : Int
    }


type alias CarrierUtilization =
    { allocator : String
    , utilization : Int
    }


type alias Model =
    { carrier_size_graph_data :
        List
            { time : String
            , carrier_size : List CarrierSize
            }
    , carrier_utilization_graph_data :
        List
            { time : String
            , carrier_utilization : List CarrierUtilization
            }
    , carrier_data :
        List AllocatorData
    }


model : Model
model =
    { carrier_size_graph_data = []
    , carrier_utilization_graph_data = []
    , carrier_data = []
    }


type Msg
    = NewAllocator (Result Http.Error Allocator)
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        NewAllocator (Ok allocator) ->
            ( updateAllocators allocator model, Cmd.none )

        NewAllocator (Err _) ->
            ( model, Cmd.none )

        Tick _ ->
            ( model, fetchdata )


updateAllocators : Allocator -> Model -> Model
updateAllocators allocators model =
    let
        cs =
            allocators.allocator.data
                |> List.map
                    (\allocator -> CarrierSize allocator.name allocator.cs)

        cu =
            allocators.allocator.data
                |> List.map
                    (\allocator -> CarrierUtilization allocator.name (floor (100 * (toFloat allocator.bs / toFloat allocator.cs))))

        graph_cs =
            model.carrier_size_graph_data ++ [ { time = allocators.allocator.time, carrier_size = cs } ]

        graph_cu =
            model.carrier_utilization_graph_data ++ [ { time = allocators.allocator.time, carrier_utilization = cu } ]

        l_graph_cs =
            if List.length graph_cs > 20 then
                List.drop 1 graph_cs
            else
                graph_cs

        l_graph_cu =
            if List.length graph_cu > 20 then
                List.drop 1 graph_cu
            else
                graph_cu
    in
    { model
        | carrier_size_graph_data = l_graph_cs
        , carrier_utilization_graph_data = l_graph_cu
        , carrier_data = allocators.allocator.data
    }


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


capitalize : Bool -> String -> String
capitalize shouldCapitalize str =
    case String.uncons str of
        Nothing ->
            str

        Just ( firstLetter, rest ) ->
            let
                newFirstLetter =
                    if shouldCapitalize then
                        Char.toUpper firstLetter
                    else
                        Char.toLower firstLetter
            in
            String.cons newFirstLetter rest


carrier_size_graph : List (Attribute msg) -> Model -> List (Attribute msg)
carrier_size_graph atr model =
    let
        first =
            List.head model.carrier_size_graph_data

        allocator_names =
            case first of
                Just graph_data ->
                    graph_data.carrier_size
                        |> List.map
                            (\item ->
                                capitalize True
                                    (if String.endsWith "_alloc" item.allocator then
                                        String.dropRight 6 item.allocator
                                     else
                                        item.allocator
                                    )
                            )

                Nothing ->
                    []

        cols =
            columns allocator_names

        data =
            model.carrier_size_graph_data
                |> List.map
                    (\graph_data ->
                        { time = graph_data.time
                        , data =
                            graph_data.carrier_size
                                |> List.map (\item -> IntWrapper (item.carrier_size // 1024))
                        }
                    )

        data_with_time =
            data |> List.map (\item -> StringWrapper (String.slice 11 19 item.time) :: item.data)

        data_string =
            graph_data cols data_with_time
    in
    if List.isEmpty data then
        []
    else
        atr ++ [ attribute "data" data_string ]


carrier_utilization_graph : List (Attribute msg) -> Model -> List (Attribute msg)
carrier_utilization_graph atr model =
    let
        first =
            List.head model.carrier_utilization_graph_data

        allocator_names =
            case first of
                Just graph_data ->
                    graph_data.carrier_utilization
                        |> List.map
                            (\item ->
                                capitalize True
                                    (if String.endsWith "_alloc" item.allocator then
                                        String.dropRight 6 item.allocator
                                     else
                                        item.allocator
                                    )
                            )

                Nothing ->
                    []

        cols =
            columns allocator_names

        data =
            model.carrier_utilization_graph_data
                |> List.map
                    (\graph_data ->
                        { time = graph_data.time
                        , data =
                            graph_data.carrier_utilization
                                |> List.map (\item -> IntWrapper item.utilization)
                        }
                    )

        data_with_time =
            data |> List.map (\item -> StringWrapper (String.slice 11 19 item.time) :: item.data)

        data_string =
            graph_data cols data_with_time
    in
    if List.isEmpty data then
        []
    else
        atr ++ [ attribute "data" data_string ]


view : Model -> Html Msg
view model =
    let
        attr =
            [ attribute "type" "line"
            , attribute "options" "{\"chartArea\": {\"height\": \"100%\"}, \"vAxis\": {\"textPosition\": \"in\"}, \"hAxis\": {\"maxAlternation\":1, \"textPosition\": \"in\"}}"
            , attribute "style" "width:100%; margin:auto"
            ]
    in
    [ cell [ Material.Grid.size Desktop 4, Material.Grid.size All 12, css "min-width" "375px", css "margin-left" "auto", css "margin-right" "auto" ]
        [ Card.view
            [ Elevation.e2, css "width" "100%" ]
            [ Card.title [] [ Card.head [] [ text "Memory Allocations" ] ]
            , Card.text [ css "width" "100%", css "padding" "0" ]
                [ Table.table [ css "width" "100%", css "border" "0" ]
                    [ Table.thead []
                        [ Table.tr []
                            [ Table.th [ Table.numeric ] [ text "Allocator Type" ]
                            , Table.th [] [ text "Block size(KB)" ]
                            , Table.th [] [ text "Carrier size(KB)" ]
                            ]
                        ]
                    , Table.tbody []
                        (model.carrier_data
                            |> List.map
                                (\item ->
                                    Table.tr []
                                        [ Table.td [ Table.numeric ]
                                            [ text
                                                (capitalize True
                                                    (if String.endsWith "_alloc" item.name then
                                                        String.dropRight 6 item.name
                                                     else
                                                        item.name
                                                    )
                                                )
                                            ]
                                        , Table.td [] [ text (toString item.bs) ]
                                        , Table.td [] [ text (toString item.cs) ]
                                        ]
                                )
                        )
                    ]
                ]
            ]
        ]
    , cell [ Material.Grid.size Desktop 8, Material.Grid.size All 12, css "margin-left" "auto", css "margin-right" "auto" ]
        [ Card.view [ Elevation.e2, css "width" "100%", css "margin-bottom" "10px" ]
            [ Card.title [] [ Card.head [] [ text "Carrier Size(MB)" ] ]
            , Card.text [] [ googleChart (carrier_size_graph attr model) [] ]
            ]
        , Card.view [ Elevation.e2, css "width" "100%", css "margin-bottom" "10px" ]
            [ Card.title [] [ Card.head [] [ text "Carrier Utilization(%)" ] ]
            , Card.text [] [ googleChart (carrier_utilization_graph attr model) [] ]
            ]
        ]
    ]
        |> grid []
        |> Views.Page.body


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every second Tick
