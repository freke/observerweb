port module Pages.Applications exposing (..)

--
-- TODO: Test this https://github.com/erkal/elm-dagre
--

import Dict exposing (Dict)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html exposing (Html, div, program)
import Html.Events exposing (on)
import Http
import Json.AppsData as AppsData exposing (AppInfo, Apps, Children, ProcessInfoApp, getAppInfo, getApps)
import Json.Decode as Decode
import Json.WheelEvent exposing (WheelEvent, wheelEvent)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Elevation as Elevation
import Material.Grid as Grid
import Material.Options as Options exposing (css)
import Mouse exposing (Position)
import Svg exposing (Svg, circle, line, path, rect, svg)
import Svg.Attributes exposing (class, cx, cy, d, fill, fontSize, height, id, opacity, preserveAspectRatio, r, rx, ry, stroke, strokeDasharray, strokeLinejoin, strokeWidth, style, textAnchor, transform, viewBox, width, x, x1, x2, y, y1, y2)
import Views.Page


type alias Model =
    { vertices :
        Dict VertexId
            { name : String
            , width : Float
            , height : Float
            , position : Point
            }
    , edges :
        Dict EdgeName
            { middlePoint : Point
            , label : String
            }
    , apps : Maybe Apps
    , app : Maybe String
    , mdl : Material.Model
    , width : Int
    , height : Int
    , scale : Float
    , position : Position
    , drag : Maybe Drag
    }


type alias Drag =
    { start : Position
    , current : Position
    }


type alias VertexName =
    String


type alias VertexId =
    String


type alias EdgeName =
    ( VertexId, VertexId )


type alias Point =
    ( Float, Float )


initialModel : Model
initialModel =
    { vertices = Dict.empty
    , edges = Dict.empty
    , apps = Nothing
    , app = Nothing
    , mdl = Material.model
    , width = 1080
    , height = 768
    , scale = 0.5
    , position = Position 0 0
    , drag = Nothing
    }


port toDagre : ( VerticesToDagre, EdgesToDagre ) -> Cmd msg


type alias VerticesToDagre =
    List
        { vertexId : VertexId
        , vertexName : VertexName
        , width : Float
        , height : Float
        }


type alias EdgesToDagre =
    List
        { source : VertexId
        , target : VertexId
        , label : String
        }


textWidth : String -> Float
textWidth string =
    toFloat (String.length string * 8 + 14)


makeVertexForDagre : ( VertexId, VertexName ) -> { vertexId : VertexId, vertexName : VertexName, width : Float, height : Float }
makeVertexForDagre ( id, name ) =
    { vertexId = id
    , vertexName = name
    , width = textWidth name
    , height = 30
    }



-- UPDATE


port fromDagre : (DataFromDagre -> msg) -> Sub msg


type alias DataFromDagre =
    { vertices :
        List
            { vertexId : VertexId
            , vertexName : VertexName
            , position : { x : Float, y : Float }
            }
    , edges :
        List
            { source : VertexId
            , target : VertexId
            , middlePoint : { x : Float, y : Float }
            , label : String
            }
    , width : Int
    , height : Int
    }


type Msg
    = Set DataFromDagre
    | NewApps (Result Http.Error Apps)
    | NewAppInfo (Result Http.Error AppInfo)
    | Mdl (Material.Msg Msg)
    | ChangeAppClickMsg String
    | DragStart Position
    | DragAt Position
    | DragEnd Position
    | Wheel WheelEvent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragStart xy ->
            ( { model | drag = Just (Drag xy xy) }, Cmd.none )

        DragAt xy ->
            ( { model | drag = Maybe.map (\{ start } -> Drag start xy) model.drag }, Cmd.none )

        DragEnd _ ->
            ( { model | position = getPosition model, drag = Nothing }, Cmd.none )

        Wheel wheel ->
            let
                scale =
                    if wheel.deltaY > 0 then
                        model.scale - (model.scale * 0.1)
                    else
                        model.scale + (model.scale * 0.1)
            in
            ( { model | scale = scale }, Cmd.none )

        NewApps (Ok apps) ->
            ( { model | apps = Just apps }, Cmd.none )

        NewAppInfo (Err e) ->
            ( model, Cmd.none )

        NewAppInfo (Ok app_info) ->
            let
                graph =
                    appGraph app_info.app

                links =
                    graph
                        |> Graph.edges
                        |> List.filterMap
                            (\edge ->
                                Just { source = toString edge.from, target = toString edge.to, label = edge.label }
                            )
            in
            ( model
            , toDagre
                ( Graph.nodes graph |> List.map (\n -> ( toString n.id, n.label )) |> List.map makeVertexForDagre
                , links
                )
            )

        NewApps (Err _) ->
            ( model, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        ChangeAppClickMsg app ->
            ( { model
                | app = Just app
                , position = Position 0 0
                , drag = Nothing
              }
            , fetchdata (Just app)
            )

        Set dataFromDagre ->
            { model
                | vertices =
                    dataFromDagre.vertices
                        |> List.foldr
                            (\v ->
                                Dict.insert v.vertexId
                                    { name = v.vertexName
                                    , width = textWidth v.vertexName
                                    , height = 30
                                    , position = v.position |> (\{ x, y } -> ( x, y ))
                                    }
                            )
                            Dict.empty
                , edges =
                    dataFromDagre.edges
                        |> List.foldr
                            (\e ->
                                Dict.insert ( e.source, e.target )
                                    { middlePoint = e.middlePoint |> (\{ x, y } -> ( x, y )), label = e.label }
                            )
                            Dict.empty
                , width = dataFromDagre.width
                , height = dataFromDagre.height
            }
                ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        default =
            [ Material.subscriptions Mdl model
            , fromDagre Set
            ]
    in
    case model.drag of
        Nothing ->
            Sub.batch default

        Just _ ->
            Sub.batch (default ++ [ Mouse.moves DragAt, Mouse.ups DragEnd ])


view : Model -> Html Msg
view model =
    Card.view [ Elevation.e2, css "margin" "auto", css "width" "100%" ]
        ([ Card.text [ css "max-height" "60vh", css "user-select" "none" ] [ viewGraph model ] ]
            ++ [ Card.actions [ Card.border ] [ view_apps model |> Grid.grid [] ]
               ]
        )
        |> Views.Page.body


viewGraph : Model -> Html Msg
viewGraph model =
    let
        realPosition =
            getPosition model
    in
    Svg.svg
        [ onMouseDown
        , onWheel
        , style "min-height: 60vh"
        , viewBox ("-10 -10 " ++ toString (model.width + 20) ++ " " ++ toString (model.height + 20))
        , preserveAspectRatio "xMinYMin meet"
        ]
        [ Svg.g [ transform ("scale(" ++ toString model.scale ++ ") translate(" ++ toString realPosition.x ++ " " ++ toString realPosition.y ++ ")") ]
            [ drawEdges model
            , drawVertices model
            ]
        ]


drawEdges : Model -> Html a
drawEdges model =
    let
        es =
            model.edges
                |> Dict.map (drawEdge model)
                |> Dict.values
    in
    Svg.g [ id "edges" ] es


drawEdge : Model -> EdgeName -> { middlePoint : Point, label : String } -> Html a
drawEdge model ( s, t ) { middlePoint, label } =
    case ( Dict.get s model.vertices, Dict.get t model.vertices ) of
        ( Just v, Just w ) ->
            let
                ( vx, vy ) =
                    v.position

                ( wx, wy ) =
                    w.position

                ( qx, qy ) =
                    middlePoint

                c =
                    case label of
                        "link" ->
                            "black"

                        "monitor" ->
                            "LightBlue"

                        e ->
                            Debug.crash "Unkown link type" e
            in
            Svg.g []
                [ path
                    [ stroke c
                    , strokeWidth "2"
                    , fill "transparent"
                    , d ("M" ++ String.join " " [ toString vx, toString vy ] ++ "Q" ++ String.join " " [ toString qx, toString qy, toString wx, toString wy ])
                    ]
                    []
                ]

        _ ->
            Debug.crash ""


drawVertices : Model -> Html a
drawVertices model =
    let
        drawVertex id { name, width, height, position } =
            Svg.g
                [ transform ("translate" ++ toString position) ]
                [ rect
                    [ Svg.Attributes.width (toString width)
                    , Svg.Attributes.height (toString height)
                    , x (toString (-width / 2))
                    , y (toString (-height / 2))
                    , fill "#3f51b5"
                    , rx "15"
                    , ry "15"
                    ]
                    []
                , Svg.text_
                    [ fill "white"
                    , textAnchor "middle"
                    , y "6"
                    ]
                    [ Svg.text name ]
                ]

        vs =
            model.vertices
                |> Dict.map drawVertex
                |> Dict.values
    in
    Svg.g [ id "vertices" ] vs


init : Model
init =
    initialModel


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
                        Grid.cell [ Grid.size Grid.All 3 ]
                            [ Button.render Mdl
                                [ index ]
                                model.mdl
                                (options item.name)
                                [ Svg.text <| item.name ++ " (" ++ item.vsn ++ ")" ]
                            ]
                    )


pidToId : List ( comparable, a ) -> Dict comparable comparable -> Dict comparable number
pidToId nodes ids =
    case nodes of
        [] ->
            ids

        ( pid, name ) :: rest ->
            pidToId rest (Dict.insert pid (getIdFromPid pid ids) ids)


getIdFromPid : comparable -> Dict comparable comparable -> number
getIdFromPid pid ids =
    case Dict.get pid ids of
        Just i ->
            i

        Nothing ->
            Maybe.withDefault -1 (List.maximum <| Dict.values ids) + 1


appInfoToEdges : ProcessInfoApp -> List ( String, String, String )
appInfoToEdges app =
    linkToParent app.pid app.children ++ List.concatMap (\c -> appInfoToEdges c) (AppsData.unwrapChildren app.children) ++ List.map (\m -> ( app.pid, m.pid, "monitor" )) app.monitors


appInfoToNodes : ProcessInfoApp -> List ( String, String )
appInfoToNodes app =
    ( app.pid, app.name ) :: List.concatMap (\c -> appInfoToNodes c) (AppsData.unwrapChildren app.children) ++ List.map (\m -> ( m.pid, m.pid )) app.monitors


linkToParent : String -> Children -> List ( String, String, String )
linkToParent parent children =
    List.map (\child -> ( parent, child.pid, "link" )) (AppsData.unwrapChildren children)


appGraph : ProcessInfoApp -> Graph String String
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
            List.map (\( from_pid, to_pid, link_type ) -> Edge (pid_to_id from_pid id_map) (pid_to_id to_pid id_map) link_type)
    in
    Graph.fromNodesAndEdges (make_nodes get_ids_nodes nodes) (make_edges get_ids_nodes edges)


onMouseDown : Html.Attribute Msg
onMouseDown =
    on "mousedown" (Decode.map DragStart Mouse.position)


onWheel : Html.Attribute Msg
onWheel =
    on "wheel" (Decode.map Wheel wheelEvent)


getPosition : Model -> Position
getPosition model =
    case model.drag of
        Nothing ->
            model.position

        Just { start, current } ->
            Position
                (model.position.x + truncate (toFloat (current.x - start.x) / model.scale))
                (model.position.y + truncate (toFloat (current.y - start.y) / model.scale))
