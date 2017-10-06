module Pages.Applications exposing (..)

import Graph exposing (Edge, Graph, Node, NodeId)
import Html exposing (Html)
import Http
import IntDict
import Json.AppsData exposing (Apps, getApps)
import List exposing (range)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Elevation as Elevation
import Material.Grid as Grid
import Material.Options as Options exposing (css)
import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)
import Time exposing (Time, second)
import Views.Page
import Visualization.Force as Force exposing (State)


screenWidth : Float
screenWidth =
    1000


screenHeight : Float
screenHeight =
    500


type alias CustomNode =
    { rank : Int, name : String }


type alias Entity =
    Force.Entity NodeId { value : CustomNode }


type alias Model =
    { app_graph_data : Graph String String
    , apps : Maybe Apps
    , mdl : Material.Model
    }


type Msg
    = ChangeAppClickMsg String
    | NewApps (Result Http.Error Apps)
    | Mdl (Material.Msg Msg)
    | Tick Time


init : Model
init =
    { app_graph_data = appGraph
    , apps = Nothing
    , mdl = Material.model
    }


fetchdata : Cmd Msg
fetchdata =
    Http.send NewApps getApps


updateGraphWithList : Graph Entity String -> List Entity -> Graph Entity String
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
    { nodeCtx | node = { node | label = value } }


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        ChangeAppClickMsg app ->
            ( model, Cmd.none )

        NewApps (Ok apps) ->
            ( { model | apps = Just apps }, Cmd.none )

        NewApps (Err _) ->
            ( model, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        Tick _ ->
            ( model, fetchdata )


linkElement : Graph Entity String -> Edge String -> Svg msg
linkElement graph edge =
    let
        retrieveEntity =
            Maybe.withDefault (Force.entity 0 (CustomNode 0 "")) << Maybe.map (.node >> .label)

        source =
            retrieveEntity <| Graph.get edge.from graph

        target =
            retrieveEntity <| Graph.get edge.to graph

        color =
            if edge.label == "link" then
                "#000000"
            else
                "#0044ff"
    in
    line
        [ strokeWidth "1"
        , stroke color
        , x1 (toString source.x)
        , y1 (toString source.y)
        , x2 (toString target.x)
        , y2 (toString target.y)
        ]
        []


nodeSize node =
    g [ transform "translate(-40,-20)" ]
        [ rect
            [ x (toString node.x)
            , y (toString node.y)
            , stroke "black"
            , fill "white"
            , strokeWidth "1px"
            , width "100px"
            , height "35px"
            , rx "5"
            , ry "5"
            ]
            []
        , text_
            [ x (toString (node.x + 10))
            , y (toString (node.y + 16))
            ]
            [ text node.value.name
            , Svg.title [] [ text node.value.name ]
            ]
        , text_
            [ x (toString (node.x + 10))
            , y (toString (node.y + 28))
            , fontSize "10"
            ]
            [ text "TEST"
            , Svg.title [] [ text "TEST" ]
            ]
        ]


nodeElement node =
    nodeSize node.label


force_graph : Graph String String -> Graph Entity String
force_graph model =
    let
        str label =
            if label == "link" then
                Just 2
            else
                Nothing

        graph =
            Graph.mapContexts
                (\({ node, incoming, outgoing } as ctx) ->
                    { ctx | node = { label = Force.entity node.id (CustomNode (IntDict.size incoming + IntDict.size outgoing) node.label), id = node.id } }
                )
                model

        links =
            graph
                |> Graph.edges
                |> List.filterMap
                    (\{ from, to, label } ->
                        if label == "link" then
                            Just { source = from, target = to, distance = 120, strength = Just 2 }
                        else
                            Nothing
                    )

        forces =
            [ Force.customLinks 1 links
            , Force.manyBodyStrength -60 <| List.map .id <| Graph.nodes graph
            , Force.center (screenWidth / 2) (screenHeight / 2)
            ]
    in
    updateGraphWithList graph (Force.computeSimulation (Force.simulation forces) <| List.map .label <| Graph.nodes graph)


view_apps : Model -> List (Grid.Cell Msg)
view_apps model =
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
                                [ Button.ripple
                                , Button.raised
                                , Options.onClick (ChangeAppClickMsg item.name)
                                ]
                                [ text <| item.name ++ " (" ++ item.vsn ++ ")" ]
                            ]
                    )


view : Model -> Html Msg
view model =
    let
        graph =
            force_graph model.app_graph_data
    in
    Card.view [ Elevation.e2, css "margin" "auto", css "width" "100%" ]
        [ Card.text []
            [ svg
                [ viewBox ("0 0 " ++ toString screenWidth ++ " " ++ toString screenHeight) ]
                [ g [ class "links" ] <| List.map (linkElement graph) <| Graph.edges graph
                , g [ class "nodes" ] <| List.map nodeElement <| Graph.nodes graph
                ]
            ]
        , Card.actions [ Card.border ] [ view_apps model |> Grid.grid [] ]
        ]
        |> Views.Page.body


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (15 * second) Tick
        , Material.subscriptions Mdl model
        ]


appGraph : Graph String String
appGraph =
    Graph.fromNodesAndEdges
        [ Node 0 "init"
        , Node 1 "<0.1.0>"
        , Node 2 "<0.2.0>"
        , Node 3 "main_sup"
        , Node 4 "app"
        , Node 5 "sup"
        , Node 6 "w1"
        , Node 7 "w2"
        , Node 8 "w3"
        ]
        [ Edge 0 1 "link"
        , Edge 1 2 "link"
        , Edge 2 3 "link"
        , Edge 3 4 "link"
        , Edge 3 5 "link"
        , Edge 5 6 "link"
        , Edge 5 7 "link"
        , Edge 5 8 "link"
        , Edge 4 6 "monitor"
        , Edge 4 7 "monitor"
        , Edge 4 8 "monitor"
        ]
