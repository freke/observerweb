module Pages.Process exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, value)
import Http
import Json.ProcessData exposing (Process, Processes, getProcesses)
import Material
import Material.Button as Button
import Material.Grid exposing (..)
import Material.Icon as Icon
import Material.List as Lists
import Material.Options as Options exposing (css, onClick)
import Material.Typography as Typo
import Navigation
import Routing
import Time exposing (Time, second)
import Views.Page


fetchdata : Cmd Msg
fetchdata =
    Http.send NewProcesses getProcesses


type alias Model =
    { id : String
    , process : Maybe Process
    , mdl : Material.Model
    }


init : String -> Model
init id =
    { id = id
    , process = Nothing
    , mdl = Material.model
    }


type Msg
    = NewProcesses (Result Http.Error Processes)
    | BackMsg
    | Mdl (Material.Msg Msg)
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        NewProcesses (Ok processes) ->
            ( updateProcesses processes model, Cmd.none )

        NewProcesses (Err _) ->
            ( model, Cmd.none )

        BackMsg ->
            ( model, Navigation.back 1 )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        Tick _ ->
            ( model, fetchdata )


updateProcesses : Processes -> Model -> Model
updateProcesses processes model =
    let
        process =
            List.head (List.filter (\i -> i.registered_name == model.id || i.pid == model.id) processes.processes)
    in
    { model | process = process }


overview_info : Process -> Cell Msg
overview_info process =
    cell [ Material.Grid.size All 4 ]
        [ Options.styled p
            [ Typo.title ]
            [ text "Overview"
            , Lists.ul []
                [ Lists.li [ Lists.withSubtitle ]
                    [ Lists.content []
                        [ text "Process id"
                        , Lists.subtitle [] [ text process.pid ]
                        ]
                    ]
                , Lists.li [ Lists.withSubtitle ]
                    [ Lists.content []
                        [ text "Registered name"
                        , Lists.subtitle [] [ text process.registered_name ]
                        ]
                    ]
                , Lists.li [ Lists.withSubtitle ]
                    [ Lists.content []
                        [ text "Status"
                        , Lists.subtitle [] [ text process.status ]
                        ]
                    ]
                , Lists.li [ Lists.withSubtitle ]
                    [ Lists.content []
                        [ text "Message queue length"
                        , Lists.subtitle [] [ text process.message_queue_len ]
                        ]
                    ]
                , Lists.li [ Lists.withSubtitle ]
                    [ Lists.content []
                        [ text "Group leader"
                        , Lists.subtitle [] [ text process.group_leader ]
                        ]
                    ]
                ]
            ]
        ]


memory_info : Process -> Cell Msg
memory_info process =
    cell [ Material.Grid.size All 4 ]
        [ Options.styled p
            [ Typo.title ]
            [ text "Memory"
            , Lists.ul []
                [ Lists.li [ Lists.withSubtitle ]
                    [ Lists.content []
                        [ text "Total"
                        , Lists.subtitle [] [ text process.memory ]
                        ]
                    ]
                , Lists.li [ Lists.withSubtitle ]
                    [ Lists.content []
                        [ text "Total heap size"
                        , Lists.subtitle [] [ text process.total_heap_size ]
                        ]
                    ]
                , Lists.li [ Lists.withSubtitle ]
                    [ Lists.content []
                        [ text "Stack size"
                        , Lists.subtitle [] [ text process.stack_size ]
                        ]
                    ]
                , Lists.li [ Lists.withSubtitle ]
                    [ Lists.content []
                        [ text "GC min heap size"
                        , Lists.subtitle [] [ text process.garbage_collection.min_heap_size ]
                        ]
                    ]
                , Lists.li [ Lists.withSubtitle ]
                    [ Lists.content []
                        [ text "GC fullsweep after"
                        , Lists.subtitle [] [ text process.garbage_collection.fullsweep_after ]
                        ]
                    ]
                ]
            ]
        ]


find_prosess_from_pid : Maybe Processes -> String -> Maybe Process
find_prosess_from_pid processes pid =
    let
        is_pid s =
            String.startsWith "<" s && String.endsWith ">" s

        find_prosess =
            case ( processes, is_pid pid ) of
                ( Just processes, True ) ->
                    List.filter (\p -> p.pid == pid) processes.processes

                ( Just processes, False ) ->
                    List.filter (\p -> p.registered_name == pid) processes.processes

                ( Nothing, _ ) ->
                    []
    in
    List.head find_prosess


links_info : Process -> Cell Msg
links_info process =
    let
        links =
            process.links
                |> List.map
                    (\item ->
                        Lists.li [] [ Lists.content [] [ a [ Routing.processPath item |> href ] [ text item ] ] ]
                    )
    in
    cell [ Material.Grid.size All 4 ]
        [ Options.styled p
            [ Typo.title ]
            [ text "Links"
            , Lists.ul [] links
            ]
        ]


ancestors_info : Process -> Cell Msg
ancestors_info process =
    let
        ancestors =
            process.ancestors
                |> List.map
                    (\item ->
                        Lists.li [] [ Lists.content [] [ a [ Routing.processPath item |> href ] [ text item ] ] ]
                    )
    in
    cell [ Material.Grid.size All 4 ]
        [ Options.styled p
            [ Typo.title ]
            [ text "Ancestors"
            , Lists.ul [] ancestors
            ]
        ]


monitors_info : Process -> Cell Msg
monitors_info process =
    let
        ancestors =
            process.monitors
                |> List.map
                    (\item ->
                        Lists.li [] [ Lists.content [] [ text item ] ]
                    )
    in
    cell [ Material.Grid.size All 4 ]
        [ Options.styled p
            [ Typo.title ]
            [ text "Monitors"
            , Lists.ul [] ancestors
            ]
        ]


view : Model -> Html Msg
view model =
    let
        name item =
            case item.registered_name of
                "" ->
                    item.initial_call

                n ->
                    n
    in
    case model.process of
        Nothing ->
            Options.styled p
                [ css "width" "100%"
                , css "margin" "16px"
                ]
                [ Options.styled p
                    [ Typo.headline ]
                    [ Button.render Mdl
                        [ 0, 0 ]
                        model.mdl
                        [ Button.icon, Button.ripple, onClick BackMsg ]
                        [ Icon.i "reply" ]
                    , text "Process information"
                    ]
                , Options.styled p
                    [ Typo.subhead ]
                    [ text "Not found" ]
                ]
                |> Views.Page.body

        Just process ->
            Options.styled p
                [ css "width" "100%"
                , css "margin" "16px"
                ]
                [ Options.styled p
                    [ Typo.headline ]
                    [ Button.render Mdl
                        [ 0, 0 ]
                        model.mdl
                        [ Button.icon, Button.ripple, onClick BackMsg ]
                        [ Icon.i "reply" ]
                    , text "Process information"
                    ]
                , Options.styled p
                    [ Typo.subhead ]
                    [ text (String.join " " [ process.registered_name, process.pid ])
                    ]
                , Options.styled div
                    []
                    [ [ overview_info process
                      , memory_info process
                      , links_info process
                      , ancestors_info process
                      , monitors_info process
                      ]
                        |> grid []
                    ]
                ]
                |> Views.Page.body


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (second * 5) Tick
