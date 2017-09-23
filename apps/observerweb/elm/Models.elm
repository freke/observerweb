module Models exposing (Model, Page(..), initialModel)

import Json.NodesData exposing (ConnectNodeResponse)
import Material
import Material.Snackbar as Snackbar
import Pages.About as About
import Pages.Applications as Applications
import Pages.LoadCharts as LoadCharts
import Pages.MemoryAllocators as MemoryAllocators
import Pages.Process as Process
import Pages.Processes as Processes
import Pages.System as System
import Pages.TableViewer as TableViewer
import Pages.TraceOverview as TraceOverview


type Page
    = NotFound
    | About About.Model
    | Applications Applications.Model
    | LoadCharts LoadCharts.Model
    | MemoryAllocators MemoryAllocators.Model
    | Processes Processes.Model
    | Process Process.Model
    | System System.Model
    | TableViewer TableViewer.Model
    | TraceOverview TraceOverview.Model


type alias Model =
    { mdl : Material.Model
    , snackbar : Snackbar.Model ConnectNodeResponse
    , tab : Page
    , nodes : List String
    , selectedNode : Maybe String
    , title : String
    , connectNode :
        { nodeName : String
        , nodeCookie : String
        }
    }


initialModel : Page -> Model
initialModel tab =
    { mdl = Material.model
    , snackbar = Snackbar.model
    , tab = tab
    , nodes = []
    , selectedNode = Nothing
    , title = ""
    , connectNode = { nodeName = "", nodeCookie = "" }
    }
