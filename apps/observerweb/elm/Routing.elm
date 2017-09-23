module Routing
    exposing
        ( Route(..)
        , aboutPath
        , applicationsPath
        , loadChartsPath
        , memoryAllocatorsPath
        , parseLocation
        , processPath
        , processesPath
        , systemPath
        , tableViewerPath
        , traceOverviewPath
        )

import Navigation exposing (Location)
import UrlParser exposing (..)


type alias ProcessId =
    String


type Route
    = AboutRoute
    | ApplicationsRoute
    | LoadChartsRoute
    | MemoryAllocatorsRoute
    | SystemRoute
    | TableViewerRoute
    | TraceOverviewRoute
    | ProcessesRoute
    | ProcessRoute ProcessId
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
        , map SystemRoute (s "system")
        , map TableViewerRoute (s "table_viewer")
        , map TraceOverviewRoute (s "trace_overview")
        ]


parseLocation : Location -> Route
parseLocation location =
    case parseHash matchers location of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


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


systemPath : String
systemPath =
    "#system"


tableViewerPath : String
tableViewerPath =
    "#table_viewer"


traceOverviewPath : String
traceOverviewPath =
    "#trace_overview"
