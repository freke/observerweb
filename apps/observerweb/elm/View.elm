module View exposing (view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (alt, href, src)
import Material.Button as Button
import Material.Dialog as Dialog
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Menu as Menu
import Material.Options as Options exposing (cs, css, when)
import Material.Scheme
import Material.Snackbar as Snackbar
import Material.Textfield as Textfield
import Models exposing (Model)
import Msgs exposing (Msg)
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
import Routing


view : Model -> Html Msg
view model =
    Layout.render Msgs.Mdl
        model.mdl
        [ Layout.fixedHeader
        , Layout.fixedDrawer
        , Layout.fixedTabs
        ]
        { drawer = [ drawerHeader model, drawer model ]
        , header = header model
        , main = [ dialog model, page model, Snackbar.view model.snackbar |> Html.map Msgs.Snackbar ]
        , tabs = ( [], [] ) --( tabTitles, [ ] )
        }
        |> Material.Scheme.top


page : Model -> Html Msg
page model =
    case model.tab of
        Models.About m ->
            About.view m |> Html.map Msgs.AboutMsg

        Models.Applications m ->
            Applications.view m |> Html.map Msgs.ApplicationsMsg

        Models.LoadCharts m ->
            LoadCharts.view m |> Html.map Msgs.LoadChartsMsg

        Models.MemoryAllocators m ->
            MemoryAllocators.view m |> Html.map Msgs.MemoryAllocatorsMsg

        Models.System m ->
            System.view m |> Html.map Msgs.SystemMsg

        Models.TableViewer m ->
            TableViewer.view m |> Html.map Msgs.TableViewerMsg

        Models.TableData m ->
            TableData.view m |> Html.map Msgs.TableDataMsg

        Models.TraceOverview m ->
            TraceOverview.view m |> Html.map Msgs.TraceOverviewMsg

        Models.Processes m ->
            Processes.view m |> Html.map Msgs.ProcessesMsg

        Models.Process m ->
            Process.view m |> Html.map Msgs.ProcessMsg

        Models.Ports m ->
            Ports.view m |> Html.map Msgs.PortsMsg

        Models.NotFound ->
            notFoundView


notFoundView : Html msg
notFoundView =
    div []
        [ text "Not found"
        ]


header : Models.Model -> List (Html Msg)
header model =
    let
        title =
            model.title
    in
    [ Layout.row
        [ Options.nop
        , css "transition" "height 333ms ease-in-out 0s"
        ]
        [ Layout.title [] [ text "ObserverWeb - ", text title ]
        , Layout.spacer
        , Layout.navigation []
            [ Layout.link []
                [ case model.selectedNode of
                    Just node ->
                        text node

                    Nothing ->
                        text "Nodes"
                ]
            , Layout.spacer
            , Menu.render Msgs.Mdl
                [ 0 ]
                model.mdl
                [ Menu.ripple
                , Menu.bottomRight
                , Menu.icon "arrow_drop_down"
                ]
                (List.map
                    (\node ->
                        Menu.item
                            [ Menu.onSelect (Msgs.ChangeNode (Just node)) ]
                            [ text node ]
                    )
                    model.nodes
                    ++ [ Menu.item
                            [ Dialog.openOn "click" ]
                            [ Icon.view "add" [], text "Connect Node..." ]
                       ]
                )
            ]
        , Layout.navigation []
            [ Layout.link
                [ Layout.href "https://github.com/freke/observerweb" ]
                [ Options.styled Html.img
                    [ Options.attribute <| src "img/github.svg"
                    , Options.attribute <| alt "github"
                    , css "width" "24px"
                    , css "height" "24px"
                    ]
                    []
                ]
            ]
        ]
    ]


drawerHeader : Models.Model -> Html Msg
drawerHeader model =
    Layout.navigation
        [ css "padding-left" "8%"
        , css "padding-right" "8%"
        ]
        [ Layout.title [] [ text "ObserverWeb" ]
        , Layout.title []
            [ Options.styled Html.img
                [ Options.attribute <| src "img/erlang-official.svg"
                , css "width" "48px"
                , css "height" "48px"
                , css "border-radius" "24px"
                ]
                []
            ]
        ]


drawers : List (Html Msg)
drawers =
    let
        icon name =
            Icon.view name [ css "width" "40px" ]
    in
    [ Layout.link
        [ Layout.href Routing.systemPath
        , Options.onClick (Layout.toggleDrawer Msgs.Mdl)
        ]
        [ icon "assessment", text "System" ]
    , Layout.link
        [ Layout.href Routing.loadChartsPath
        , Options.onClick (Layout.toggleDrawer Msgs.Mdl)
        ]
        [ icon "trending_up", text "Load Charts" ]
    , Layout.link
        [ Layout.href Routing.memoryAllocatorsPath
        , Options.onClick (Layout.toggleDrawer Msgs.Mdl)
        ]
        [ icon "memory", text "Memory Allocators" ]
    , Layout.link
        [ Layout.href Routing.applicationsPath
        , Options.onClick (Layout.toggleDrawer Msgs.Mdl)
        ]
        [ icon "devices_other", text "Applications" ]
    , Layout.link
        [ Layout.href Routing.processesPath
        , Options.onClick (Layout.toggleDrawer Msgs.Mdl)
        ]
        [ icon "settings_applications", text "Processes" ]
    , Layout.link
        [ Layout.href Routing.portsPath
        , Options.onClick (Layout.toggleDrawer Msgs.Mdl)
        ]
        [ icon "usb", text "Ports" ]
    , Layout.link
        [ Layout.href Routing.tableViewerPath
        , Options.onClick (Layout.toggleDrawer Msgs.Mdl)
        ]
        [ icon "view_headline", text "Table Viewer" ]
    , Layout.link
        [ Layout.href Routing.traceOverviewPath
        , Options.onClick (Layout.toggleDrawer Msgs.Mdl)
        ]
        [ icon "bug_report", text "Trace Overview" ]
    , Layout.link
        [ Layout.href Routing.aboutPath
        , Options.onClick (Layout.toggleDrawer Msgs.Mdl)
        ]
        [ icon "info_outline", text "About" ]
    ]


drawer : Models.Model -> Html Msg
drawer model =
    Layout.navigation
        []
        drawers


dialog : Models.Model -> Html Msg
dialog model =
    Dialog.view
        []
        [ Dialog.title [] [ text "Connect node" ]
        , Dialog.content []
            [ Textfield.render Msgs.Mdl
                [ 0 ]
                model.mdl
                [ Textfield.floatingLabel
                , Textfield.label "Node name (nonode@nohost)"
                , Textfield.value model.connectNode.nodeName
                , Options.onInput Msgs.ChangeNodeNameMsg
                ]
                []
            , Textfield.render Msgs.Mdl
                [ 1 ]
                model.mdl
                [ Textfield.floatingLabel
                , Textfield.label "Secret cookie"
                , Textfield.value model.connectNode.nodeCookie
                , Options.onInput Msgs.ChangeNodeCookieMsg
                ]
                []
            ]
        , Dialog.actions []
            [ Button.render Msgs.Mdl
                [ 0 ]
                model.mdl
                [ Dialog.closeOn "click"
                , Button.flat
                ]
                [ text "Cancel" ]
            , Button.render Msgs.Mdl
                [ 1 ]
                model.mdl
                [ Dialog.closeOn "click"
                , Button.colored
                , Button.raised
                , Options.onClick Msgs.ConnectNodeMsg
                ]
                [ text "OK" ]
            ]
        ]
