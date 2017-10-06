module Main exposing (main)

import Http
import Json.NodesData exposing (ConnectNodeResponse, Nodes, connectNodeResponseDecoder, nodesDecoder)
import Material
import Material.Helpers exposing (delay, lift, map1st, map2nd, pure)
import Material.Snackbar as Snackbar
import Models
import Msgs exposing (Msg)
import Navigation
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
import Routing exposing (parseLocation)
import Time exposing (Time, millisecond)
import View exposing (view)


init : Navigation.Location -> ( Models.Model, Cmd Msg )
init location =
    let
        currentRoute =
            Routing.parseLocation location

        ( tab, c ) =
            routeToPage currentRoute

        cmd =
            Cmd.batch
                [ Cmd.map Msgs.SystemMsg System.fetchdata
                , Http.send Msgs.NewNodesMsg getNodes
                , c
                ]
    in
    ( Models.initialModel tab, cmd )


main : Program Never Models.Model Msg
main =
    Navigation.program Msgs.OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- UPDATE


update : Msg -> Models.Model -> ( Models.Model, Cmd Msg )
update msg model =
    let
        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
            ( { model | tab = toModel newModel }, Cmd.map toMsg newCmd )
    in
    case ( model.tab, msg ) of
        ( _, Msgs.Mdl msg_ ) ->
            Material.update Msgs.Mdl msg_ model

        ( _, Msgs.OnLocationChange location ) ->
            let
                newRoute =
                    parseLocation location

                ( tab, cmd ) =
                    routeToPage newRoute
            in
            ( { model | tab = tab, title = Routing.title newRoute }, cmd )

        ( Models.System sub_model, Msgs.SystemMsg a ) ->
            toPage Models.System Msgs.SystemMsg System.update a sub_model

        ( Models.LoadCharts sub_model, Msgs.LoadChartsMsg a ) ->
            toPage Models.LoadCharts Msgs.LoadChartsMsg LoadCharts.update a sub_model

        ( Models.MemoryAllocators sub_model, Msgs.MemoryAllocatorsMsg a ) ->
            toPage Models.MemoryAllocators Msgs.MemoryAllocatorsMsg MemoryAllocators.update a sub_model

        ( Models.Applications sub_model, Msgs.ApplicationsMsg a ) ->
            toPage Models.Applications Msgs.ApplicationsMsg Applications.update a sub_model

        ( Models.Process sub_model, Msgs.ProcessMsg a ) ->
            toPage Models.Process Msgs.ProcessMsg Process.update a sub_model

        ( Models.Processes sub_model, Msgs.ProcessesMsg a ) ->
            toPage Models.Processes Msgs.ProcessesMsg Processes.update a sub_model

        ( Models.Ports sub_model, Msgs.PortsMsg a ) ->
            toPage Models.Ports Msgs.PortsMsg Ports.update a sub_model

        ( Models.TableViewer sub_model, Msgs.TableViewerMsg a ) ->
            toPage Models.TableViewer Msgs.TableViewerMsg TableViewer.update a sub_model

        ( Models.TableData sub_model, Msgs.TableDataMsg a ) ->
            toPage Models.TableData Msgs.TableDataMsg TableData.update a sub_model

        ( Models.TraceOverview sub_model, Msgs.TraceOverviewMsg a ) ->
            toPage Models.TraceOverview Msgs.TraceOverviewMsg TraceOverview.update a sub_model

        ( Models.About sub_model, Msgs.AboutMsg a ) ->
            toPage Models.About Msgs.AboutMsg About.update a sub_model

        ( _, Msgs.NewNodesMsg (Ok nodes) ) ->
            ( updateNodes nodes model, Cmd.none )

        ( _, Msgs.NewNodesMsg (Err _) ) ->
            ( model, Cmd.none )

        ( _, Msgs.ChangeNodeNameMsg nodeName ) ->
            let
                connectNode =
                    model.connectNode
            in
            ( { model | connectNode = { connectNode | nodeName = nodeName } }, Cmd.none )

        ( _, Msgs.ChangeNodeCookieMsg nodeCookie ) ->
            let
                connectNode =
                    model.connectNode
            in
            ( { model | connectNode = { connectNode | nodeCookie = nodeCookie } }, Cmd.none )

        ( _, Msgs.ConnectNodeMsg ) ->
            ( model, connectNode model )

        ( _, Msgs.ChangeNode node ) ->
            case node of
                Just n ->
                    ( model, changeNode n )

                Nothing ->
                    ( model, Cmd.none )

        ( _, Msgs.ConnectNodeResponseMsg (Ok response) ) ->
            let
                selectedNode =
                    if response.connected then
                        Just response.node
                    else
                        model.selectedNode

                ( snackbar, cmd ) =
                    Snackbar.add (Snackbar.toast response <| "Node: " ++ response.node ++ " Connected: " ++ toString response.connected) model.snackbar |> map2nd (Cmd.map Msgs.Snackbar)
            in
            ( { model | snackbar = snackbar, selectedNode = selectedNode }, cmd )

        ( _, Msgs.ConnectNodeResponseMsg (Err _) ) ->
            ( model, Cmd.none )

        ( _, Msgs.Snackbar (Snackbar.Begin k) ) ->
            model |> pure

        ( _, Msgs.Snackbar (Snackbar.End k) ) ->
            model |> pure

        ( _, Msgs.Snackbar (Snackbar.Click k) ) ->
            ( model, Cmd.none )

        ( _, Msgs.Snackbar msg_ ) ->
            Snackbar.update msg_ model.snackbar
                |> map1st (\s -> { model | snackbar = s })
                |> map2nd (Cmd.map Msgs.Snackbar)

        ( _, _ ) ->
            ( model, Cmd.none )


routeToPage : Routing.Route -> ( Models.Page, Cmd Msg )
routeToPage route =
    case route of
        Routing.AboutRoute ->
            ( Models.About About.model, Cmd.none )

        Routing.ApplicationsRoute ->
            ( Models.Applications Applications.init, Cmd.map Msgs.ApplicationsMsg Applications.fetchdata )

        Routing.LoadChartsRoute ->
            ( Models.LoadCharts LoadCharts.model, Cmd.map Msgs.LoadChartsMsg LoadCharts.fetchdata )

        Routing.MemoryAllocatorsRoute ->
            ( Models.MemoryAllocators MemoryAllocators.model, Cmd.map Msgs.MemoryAllocatorsMsg MemoryAllocators.fetchdata )

        Routing.SystemRoute ->
            ( Models.System System.model, Cmd.map Msgs.SystemMsg System.fetchdata )

        Routing.TableViewerRoute ->
            ( Models.TableViewer TableViewer.model, Cmd.map Msgs.TableViewerMsg TableViewer.fetchdata )

        Routing.TableRoute id ->
            ( Models.TableData (TableData.init id), Cmd.map Msgs.TableDataMsg (TableData.fetchdata id) )

        Routing.TraceOverviewRoute ->
            ( Models.TraceOverview TraceOverview.model, Cmd.none )

        Routing.ProcessesRoute ->
            ( Models.Processes Processes.model, Cmd.map Msgs.ProcessesMsg Processes.fetchdata )

        Routing.ProcessRoute id ->
            ( Models.Process (Process.init id), Cmd.map Msgs.ProcessMsg Process.fetchdata )

        Routing.PortsRoute ->
            ( Models.Ports Ports.model, Cmd.map Msgs.PortsMsg Ports.fetchdata )

        Routing.NotFoundRoute ->
            ( Models.NotFound, Cmd.none )


updateNodes : Nodes -> Models.Model -> Models.Model
updateNodes nodes model =
    { model | nodes = nodes.nodes }


changeNode : String -> Cmd Msg
changeNode node =
    Http.send Msgs.ConnectNodeResponseMsg (Http.post "/info" (Http.stringBody "text/plain" ("action=change_node&node=" ++ node)) connectNodeResponseDecoder)


getNodes : Http.Request Nodes
getNodes =
    Http.post "/info" (Http.stringBody "text/plain" "action=get_nodes") nodesDecoder


connectNode : Models.Model -> Cmd Msg
connectNode model =
    Http.send Msgs.ConnectNodeResponseMsg
        (Http.post "/info" (Http.stringBody "text/plain" ("action=connect_node&node=" ++ model.connectNode.nodeName ++ "&cookie=" ++ model.connectNode.nodeCookie)) connectNodeResponseDecoder)


transitionLength : Time
transitionLength =
    150 * millisecond



-- SUBSCRIPTIONS


subscriptions : Models.Model -> Sub Msg
subscriptions model =
    let
        sub =
            case model.tab of
                Models.NotFound ->
                    Sub.none

                Models.About sub_model ->
                    Sub.map Msgs.AboutMsg (About.subscriptions sub_model)

                Models.Applications sub_model ->
                    Sub.map Msgs.ApplicationsMsg (Applications.subscriptions sub_model)

                Models.LoadCharts sub_model ->
                    Sub.map Msgs.LoadChartsMsg (LoadCharts.subscriptions sub_model)

                Models.MemoryAllocators sub_model ->
                    Sub.map Msgs.MemoryAllocatorsMsg (MemoryAllocators.subscriptions sub_model)

                Models.Process sub_model ->
                    Sub.map Msgs.ProcessMsg (Process.subscriptions sub_model)

                Models.Processes sub_model ->
                    Sub.map Msgs.ProcessesMsg (Processes.subscriptions sub_model)

                Models.Ports sub_model ->
                    Sub.map Msgs.PortsMsg (Ports.subscriptions sub_model)

                Models.System sub_model ->
                    Sub.map Msgs.SystemMsg (System.subscriptions sub_model)

                Models.TableViewer sub_model ->
                    Sub.map Msgs.TableViewerMsg (TableViewer.subscriptions sub_model)

                Models.TableData sub_model ->
                    Sub.map Msgs.TableDataMsg (TableData.subscriptions sub_model)

                Models.TraceOverview sub_model ->
                    Sub.map Msgs.TraceOverviewMsg (TraceOverview.subscriptions sub_model)
    in
    Sub.batch
        [ sub
        , Material.subscriptions Msgs.Mdl model
        ]
