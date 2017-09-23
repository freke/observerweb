module Pages.System exposing (..)

import Html exposing (..)
import Http
import Json.SystemData exposing (SystemInfo, Value, getSystem)
import Material.Card as Card
import Material.Elevation as Elevation
import Material.Grid exposing (..)
import Material.List as Lists
import Material.Options exposing (css)
import Time exposing (Time, second)
import Views.Page


fetchdata : Cmd Msg
fetchdata =
    Http.send NewSystem getSystem


type alias Model =
    { systemInfo : Maybe SystemInfo }


model : Model
model =
    { systemInfo = Nothing
    }


type Msg
    = NewSystem (Result Http.Error SystemInfo)
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        NewSystem (Ok systemInfo) ->
            ( { model | systemInfo = Just systemInfo }, Cmd.none )

        NewSystem (Err _) ->
            ( model, Cmd.none )

        Tick _ ->
            ( model, fetchdata )


card : Model -> String -> List Value -> Html Msg
card _ title data =
    Card.view [ Elevation.e2, css "margin" "auto", css "width" "100%" ]
        [ Card.title [] [ Card.head [] [ text title ] ]
        , Card.text []
            [ Lists.ul []
                (data
                    |> List.map
                        (\item ->
                            Lists.li [ Lists.withSubtitle ]
                                [ Lists.content []
                                    [ text item.name
                                    , Lists.subtitle [] [ text item.value ]
                                    ]
                                ]
                        )
                )
            ]
        ]


view : Model -> Html Msg
view model =
    let
        system =
            case model.systemInfo of
                Nothing ->
                    []

                Just val ->
                    val.system.data

        memory =
            case model.systemInfo of
                Nothing ->
                    []

                Just val ->
                    val.memory.data

        cpu =
            case model.systemInfo of
                Nothing ->
                    []

                Just val ->
                    val.cpu.data

        statistics =
            case model.systemInfo of
                Nothing ->
                    []

                Just val ->
                    val.statistics.data
    in
    [ cell [ Material.Grid.size All 12, Material.Grid.size Tablet 4, Material.Grid.size Desktop 3 ] [ card model "System and Architecture" system ]
    , cell [ Material.Grid.size All 12, Material.Grid.size Tablet 4, Material.Grid.size Desktop 3 ] [ card model "Memory Usage" memory ]
    , cell [ Material.Grid.size All 12, Material.Grid.size Tablet 4, Material.Grid.size Desktop 3 ] [ card model "CPU's and Threads" cpu ]
    , cell [ Material.Grid.size All 12, Material.Grid.size Tablet 4, Material.Grid.size Desktop 3 ] [ card model "Statistics" statistics ]
    ]
        |> grid []
        |> Views.Page.body


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (second * 5) Tick
