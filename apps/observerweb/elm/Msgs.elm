module Msgs exposing (Msg(..))

import Http
import Json.NodesData exposing (ConnectNodeResponse, Nodes)
import Material
import Material.Snackbar as Snackbar
import Navigation exposing (Location)
import Pages.About as About
import Pages.Applications as Applications
import Pages.LoadCharts as LoadCharts
import Pages.MemoryAllocators as MemoryAllocators
import Pages.Ports as Ports
import Pages.Process as Process
import Pages.Processes as Processes
import Pages.System as System
import Pages.TableData as TableData
import Pages.TableViewer as TableViewer
import Pages.TraceOverview as TraceOverview


type Msg
    = OnLocationChange Location
    | SystemMsg System.Msg
    | LoadChartsMsg LoadCharts.Msg
    | MemoryAllocatorsMsg MemoryAllocators.Msg
    | ApplicationsMsg Applications.Msg
    | ProcessMsg Process.Msg
    | ProcessesMsg Processes.Msg
    | PortsMsg Ports.Msg
    | TableViewerMsg TableViewer.Msg
    | TableDataMsg TableData.Msg
    | TraceOverviewMsg TraceOverview.Msg
    | AboutMsg About.Msg
    | Mdl (Material.Msg Msg)
    | ChangeNode (Maybe String)
    | NewNodesMsg (Result Http.Error Nodes)
    | ChangeNodeNameMsg String
    | ChangeNodeCookieMsg String
    | ConnectNodeMsg
    | ConnectNodeResponseMsg (Result Http.Error ConnectNodeResponse)
    | Snackbar (Snackbar.Msg ConnectNodeResponse)
