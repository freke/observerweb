module Routing
    exposing
        ( Route(..)
        , aboutPath
        , applicationsPath
        , loadChartsPath
        , memoryAllocatorsPath
        , parseLocation
        , portsPath
        , processPath
        , processesPath
        , systemPath
        , tablePath
        , tableViewerPath
        , title
        , traceOverviewPath
        )

import Navigation exposing (Location)
import UrlParser exposing (..)


type alias ProcessId =
    String


type alias TableId =
    String


type Route
    = AboutRoute
    | ApplicationsRoute
    | LoadChartsRoute
    | MemoryAllocatorsRoute
    | SystemRoute
    | TableViewerRoute
    | TableRoute TableId
    | TraceOverviewRoute
    | ProcessesRoute
    | ProcessRoute ProcessId
    | PortsRoute
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map SystemRoute top
        , map AboutRoute (s "about")
        , map ApplicationsRoute (s "applications")
        , map LoadChartsRoute (s "load_charts")
        , map MemoryAllocatorsRoute (s "memory_alloc")
        , map ProcessRoute (s "processes" </> string)
        , map ProcessesRoute (s "processes")
        , map PortsRoute (s "ports")
        , map SystemRoute (s "system")
        , map TableViewerRoute (s "table_viewer")
        , map TableRoute (s "table_viewer" </> string)
        , map TraceOverviewRoute (s "trace_overview")
        ]


parseLocation : Location -> Route
parseLocation location =
    case parseHash matchers location of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


title : Route -> String
title route =
    case route of
        AboutRoute ->
            "About"

        ApplicationsRoute ->
            "Applications"

        LoadChartsRoute ->
            "LoadCharts"

        MemoryAllocatorsRoute ->
            "MemoryAllocators"

        SystemRoute ->
            "System"

        TableViewerRoute ->
            "TableViewer"

        TableRoute _ ->
            "Table Data"

        TraceOverviewRoute ->
            "Trace Overview"

        ProcessesRoute ->
            "Processes"

        ProcessRoute _ ->
            "Process"

        PortsRoute ->
            "Ports"

        NotFoundRoute ->
            ""


aboutPath : String
aboutPath =
    "#about"


applicationsPath : String
applicationsPath =
    "#applications"


loadChartsPath : String
loadChartsPath =
    "#load_charts"


memoryAllocatorsPath : String
memoryAllocatorsPath =
    "#memory_alloc"


processPath : ProcessId -> String
processPath id =
    "#processes/" ++ id


processesPath : String
processesPath =
    "#processes"


portsPath : String
portsPath =
    "#ports"


systemPath : String
systemPath =
    "#system"


tableViewerPath : String
tableViewerPath =
    "#table_viewer"


tablePath : TableId -> String
tablePath id =
    "#table_viewer/" ++ id


traceOverviewPath : String
traceOverviewPath =
    "#trace_overview"
