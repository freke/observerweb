module Pages.Applications exposing (..)

--
-- {-| This demonstrates laying out the characters in Les Miserables
-- based on their co-occurence in a scene. Try dragging the nodes!
-- -}
--

import AnimationFrame
import Dict
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html exposing (Html)
import Html.Events exposing (on)
import Http
import Json.AppsData as AppsData exposing (AppInfo, Apps, Children, ProcessInfoApp, getAppInfo, getApps)
import Json.Decode as Decode
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Elevation as Elevation
import Material.Grid as Grid
import Material.Options as Options exposing (css)
import Mouse exposing (Position)
import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)
import Time exposing (Time, second)
import Views.Page
import Visualization.Force as Force exposing (State)


--
--
-- module Pages.Applications exposing (..)
--
-- import AnimationFrame
-- import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
-- import Html
-- import Html.Events exposing (on)
-- import Json.Decode as Decode
-- import Mouse exposing (Position)
-- import Svg exposing (..)
-- import Svg.Attributes as Attr exposing (..)
-- import Time exposing (Time)
-- import Visualization.Force as Force exposing (State)
--
-- screenWidth : Float
-- screenWidth =
--     990
--
--
-- screenHeight : Float
-- screenHeight =
--     504
--
--
-- type Msg
--     = DragStart NodeId Position
--     | DragAt Position
--     | DragEnd Position
--     | NewApps (Result Http.Error Apps)
--     | NewAppInfo (Result Http.Error AppInfo)
--     | ChangeAppClickMsg String
--     | Mdl (Material.Msg Msg)
--     | Tick Time
--
--
-- type alias Model =
--     { drag : Maybe Drag
--     , graph : Graph Entity String
--     , simulation : Force.State NodeId
--     , app : Maybe String
--     , apps : Maybe Apps
--     , mdl : Material.Model
--     }
--
--
-- type alias Drag =
--     { start : Position
--     , current : Position
--     , index : NodeId
--     }
--
--
-- type alias Entity =
--     Force.Entity NodeId { value : String }
--
--
-- init : Model
-- init =
--     let
--         graph =
--             Graph.empty
--
--         link { from, to } =
--             ( from, to )
--
--         forces =
--             [ Force.links <| List.map link <| Graph.edges graph
--             , Force.manyBody <| List.map .id <| Graph.nodes graph
--             , Force.center (screenWidth / 2) (screenHeight / 2)
--             ]
--     in
--     Model Nothing graph (Force.simulation forces) Nothing Nothing Material.model
--
--


fetchdata : Maybe String -> Cmd Msg
fetchdata app =
    let
        get_app_info_cmd =
            case app of
                Just app ->
                    Http.send NewAppInfo (getAppInfo app)

                Nothing ->
                    Cmd.none

        get_apps_cmd =
            Http.send NewApps getApps
    in
    Cmd.batch [ get_app_info_cmd, get_apps_cmd ]



--
--
-- updateNode : Position -> NodeContext Entity String -> NodeContext Entity String
-- updateNode pos nodeCtx =
--     let
--         nodeValue =
--             nodeCtx.node.label
--     in
--     updateContextWithValue nodeCtx { nodeValue | x = toFloat pos.x, y = toFloat pos.y }
--
--
-- updateContextWithValue : NodeContext Entity String -> Entity -> NodeContext Entity String
-- updateContextWithValue nodeCtx value =
--     let
--         node =
--             nodeCtx.node
--     in
--     { nodeCtx | node = { node | label = value } }
--
--
-- updateGraphWithList : Graph Entity String -> List Entity -> Graph Entity String
-- updateGraphWithList =
--     let
--         graphUpdater value =
--             Maybe.map (\ctx -> updateContextWithValue ctx value)
--     in
--     List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)
--
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TickAnimation t ->
            let
                ( newState, list ) =
                    Force.tick model.simulation <| List.map .label <| Graph.nodes model.graph

                newModel =
                    { model
                        | graph = updateGraphWithList model.graph list
                        , simulation = newState
                    }
            in
            ( newModel, Cmd.none )

        DragStart index xy_a xy ->
            ( { model | drag = Just (Drag xy xy index) }, Cmd.none )

        DragAt xy ->
            case model.drag of
                Just { start, index } ->
                    ( { model
                        | drag = Just (Drag start xy index)
                        , graph = Graph.update index (Maybe.map (updateNode xy)) model.graph
                        , simulation = Force.reheat model.simulation
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        DragEnd xy ->
            case model.drag of
                Just { start, index } ->
                    ( { model | drag = Nothing, graph = Graph.update index (Maybe.map (updateNode xy)) model.graph }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        NewApps (Ok apps) ->
            ( { model | apps = Just apps }, Cmd.none )

        NewAppInfo (Err e) ->
            ( model, Cmd.none )

        NewAppInfo (Ok app_info) ->
            let
                graph =
                    Graph.mapContexts
                        (\({ node } as ctx) ->
                            { ctx | node = { label = Force.entity node.id node.label, id = node.id } }
                        )
                        (appGraph app_info.app)

                links =
                    graph
                        |> Graph.edges
                        |> List.filterMap
                            (\{ from, to, label } ->
                                Just { source = from, target = to, distance = 100, strength = Just 2 }
                            )

                forces =
                    [ Force.customLinks 1 links
                    , Force.manyBodyStrength -10 <| List.map .id <| Graph.nodes graph
                    , Force.center (screenWidth / 2) (screenHeight / 2)
                    ]

                newModel =
                    { model
                        | graph = graph
                        , simulation = Force.simulation forces
                    }
            in
            ( newModel, Cmd.none )

        NewApps (Err _) ->
            ( model, Cmd.none )

        ChangeAppClickMsg app ->
            ( { model | app = Just app }, fetchdata (Just app) )

        Tick _ ->
            ( model, fetchdata model.app )



--
--
-- subscriptions : Model -> Sub Msg
-- subscriptions model =
--     case model.drag of
--         Nothing ->
--             -- This allows us to save resources, as if the simulation is done, there is no point in subscribing
--             -- to the rAF.
--             if Force.isCompleted model.simulation then
--                 Sub.none
--             else
--                 AnimationFrame.times Tick
--
--         Just _ ->
--             Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd, AnimationFrame.times Tick ]
--
--
-- onMouseDown : NodeId -> Attribute Msg
-- onMouseDown index =
--     on "mousedown" (Decode.map (DragStart index) Mouse.position)
--
--
-- linkElement graph edge =
--     let
--         source =
--             Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.from graph
--
--         target =
--             Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
--     in
--     line
--         [ strokeWidth "1"
--         , stroke "#aaa"
--         , x1 (toString source.x)
--         , y1 (toString source.y)
--         , x2 (toString target.x)
--         , y2 (toString target.y)
--         ]
--         []
--
--
-- nodeElement node =
--     circle
--         [ r "2.5"
--         , fill "#000"
--         , stroke "transparent"
--         , strokeWidth "7px"
--         , onMouseDown node.id
--         , cx (toString node.label.x)
--         , cy (toString node.label.y)
--         ]
--         [ Svg.title [] [ text node.label.value ] ]
--
--


view_apps : Model -> List (Grid.Cell Msg)
view_apps model =
    let
        selected_app app =
            case model.app of
                Just a ->
                    a == app

                Nothing ->
                    False

        def_opt app =
            [ Button.ripple
            , Button.raised
            , Options.onClick (ChangeAppClickMsg app)
            ]

        options app =
            if selected_app app then
                Button.colored :: def_opt app
            else
                def_opt app
    in
    case model.apps of
        Nothing ->
            []

        Just apps ->
            apps.apps
                |> List.indexedMap
                    (\index item ->
                        Grid.cell [ Grid.size Grid.All 2 ]
                            [ Button.render Mdl
                                [ index ]
                                model.mdl
                                (options item.name)
                                [ text <| item.name ++ " (" ++ item.vsn ++ ")" ]
                            ]
                    )


view : Model -> Html Msg
view model =
    let
        app_graph =
            [ Card.text []
                [ svg
                    [ viewBox ("0 0 " ++ toString screenWidth ++ " " ++ toString screenHeight) ]
                    [ g [ class "links" ] <| List.map (linkElement model.graph) <| Graph.edges model.graph
                    , g [ class "nodes" ] <| List.map nodeElement <| Graph.nodes model.graph
                    ]
                ]
            ]
    in
    Card.view [ Elevation.e2, css "margin" "auto", css "width" "100%" ]
        (app_graph
            ++ [ Card.actions [ Card.border ] [ view_apps model |> Grid.grid [] ]
               ]
        )
        |> Views.Page.body


pidToId nodes ids =
    case nodes of
        [] ->
            ids

        ( pid, name ) :: rest ->
            pidToId rest (Dict.insert pid (getIdFromPid pid ids) ids)


getIdFromPid pid ids =
    case Dict.get pid ids of
        Just i ->
            i

        Nothing ->
            Maybe.withDefault -1 (List.maximum <| Dict.values ids) + 1


appInfoToEdges : ProcessInfoApp -> List ( String, String, String )
appInfoToEdges app =
    linkToParent app.pid app.children ++ List.concatMap (\c -> appInfoToEdges c) (AppsData.unwrapChildren app.children)


appInfoToNodes : ProcessInfoApp -> List ( String, String )
appInfoToNodes app =
    ( app.pid, app.name ) :: List.concatMap (\c -> appInfoToNodes c) (AppsData.unwrapChildren app.children)


linkToParent : String -> Children -> List ( String, String, String )
linkToParent parent children =
    List.map (\child -> ( parent, child.pid, "link" )) (AppsData.unwrapChildren children)


appGraph app_info =
    let
        nodes =
            appInfoToNodes app_info

        edges =
            appInfoToEdges app_info

        get_ids_nodes =
            pidToId nodes Dict.empty

        pid_to_id pid id_map =
            Maybe.withDefault -1 (Dict.get pid id_map)

        make_nodes id_map =
            List.map (\( pid, name ) -> Node (pid_to_id pid id_map) name)

        make_edges id_map =
            List.map (\( from_pid, to_pid, _ ) -> Edge (pid_to_id from_pid id_map) (pid_to_id to_pid id_map) ())
    in
    Graph.fromNodesAndEdges (make_nodes get_ids_nodes nodes) (make_edges get_ids_nodes edges)


screenWidth : Float
screenWidth =
    1920


screenHeight : Float
screenHeight =
    1080


type Msg
    = Tick Time
    | TickAnimation Time
    | DragStart NodeId ( Float, Float ) Position
    | DragAt Position
    | DragEnd Position
    | Mdl (Material.Msg Msg)
    | ChangeAppClickMsg String
    | NewApps (Result Http.Error Apps)
    | NewAppInfo (Result Http.Error AppInfo)


type alias Model =
    { drag : Maybe Drag
    , graph : Graph Entity ()
    , simulation : Force.State NodeId
    , app : Maybe String
    , apps : Maybe Apps
    , mdl : Material.Model
    }


type alias Drag =
    { start : Position
    , current : Position
    , index : NodeId
    }


type alias Entity =
    Force.Entity NodeId { value : String }


init : Model
init =
    Model Nothing Graph.empty (Force.simulation []) Nothing Nothing Material.model


updateNode : Position -> NodeContext Entity () -> NodeContext Entity ()
updateNode pos nodeCtx =
    let
        nodeValue =
            nodeCtx.node.label
    in
    updateContextWithValue nodeCtx { nodeValue | x = toFloat pos.x, y = toFloat pos.y }


updateContextWithValue : NodeContext Entity () -> Entity -> NodeContext Entity ()
updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
    { nodeCtx | node = { node | label = value } }


updateGraphWithList : Graph Entity () -> List Entity -> Graph Entity ()
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


onMouseDown : NodeId -> ( Float, Float ) -> Attribute Msg
onMouseDown index pos =
    on "mousedown" (Decode.map (DragStart index pos) Mouse.position)


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        animation =
            if Force.isCompleted model.simulation then
                Sub.none
            else
                AnimationFrame.times TickAnimation
    in
    Sub.batch
        [ Time.every (15 * second) Tick
        , Material.subscriptions Mdl model
        , animation
        , Mouse.moves DragAt
        , Mouse.ups DragEnd
        ]


linkElement graph edge =
    let
        source =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
    in
    line
        [ strokeWidth "1"
        , stroke "#aaa"
        , x1 (toString source.x)
        , y1 (toString source.y)
        , x2 (toString target.x)
        , y2 (toString target.y)
        ]
        []


nodeElement node =
    circle
        [ r "2.5"
        , fill "#000"
        , stroke "transparent"
        , strokeWidth "7px"
        , onMouseDown node.id ( node.label.x, node.label.y )
        , cx (toString node.label.x)
        , cy (toString node.label.y)
        ]
        [ Svg.title [] [ text node.label.value ] ]
