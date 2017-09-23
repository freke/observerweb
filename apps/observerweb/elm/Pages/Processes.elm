module Pages.Processes exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, value)
import Http
import Json.ProcessData exposing (Process, Processes, getProcesses)
import Material
import Material.Card as Card
import Material.Elevation as Elevation
import Material.Options as Options exposing (css, onClick)
import Material.Table as Table
import Routing
import Time exposing (Time, second)
import Views.Page


fetchdata : Cmd Msg
fetchdata =
    Http.send NewProcesses getProcesses


type alias Model =
    { processes : Maybe Processes
    , mdl : Material.Model
    }


model : Model
model =
    { processes = Nothing
    , mdl = Material.model
    }


type Msg
    = NewProcesses (Result Http.Error Processes)
    | Mdl (Material.Msg Msg)
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        NewProcesses (Ok processes) ->
            ( updateProcesses processes model, Cmd.none )

        NewProcesses (Err _) ->
            ( model, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        Tick _ ->
            ( model, fetchdata )


updateProcesses : Processes -> Model -> Model
updateProcesses processes model =
    { model | processes = Just processes }


view : Model -> Html Msg
view model =
    let
        name item =
            case item.registered_name of
                "" ->
                    item.initial_call

                n ->
                    n

        processes =
            case model.processes of
                Nothing ->
                    [ Table.tr [] [] ]

                Just processes ->
                    processes.processes
                        |> List.map
                            (\item ->
                                Table.tr []
                                    [ Table.td [ Table.numeric ] [ a [ Routing.processPath item.pid |> href ] [ text item.pid ] ]
                                    , Table.td [ Table.numeric ] [ text (name item) ]
                                    , Table.td [ Table.numeric ] [ text item.reductions ]
                                    , Table.td [ Table.numeric ] [ text item.memory ]
                                    , Table.td [ Table.numeric ] [ text item.message_queue_len ]
                                    , Table.td [ Table.numeric ] [ text item.current_function ]
                                    ]
                            )
    in
    Card.view
        [ Elevation.e2, css "width" "100%", css "margin" "16px" ]
        [ Card.title [] [ Card.head [] [ text "Process Table" ] ]
        , Card.text [ css "width" "100%", css "padding" "0" ]
            [ Table.table [ css "width" "100%", css "border" "0" ]
                [ Table.thead []
                    [ Table.tr []
                        [ Table.th [ Table.numeric, css "width" "13%" ] [ text "Pid" ]
                        , Table.th [ Table.numeric, css "width" "24%" ] [ text "Name or Initial Func" ]
                        , Table.th [ Table.numeric, css "width" "13%" ] [ text "Reds" ]
                        , Table.th [ Table.numeric, css "width" "13%" ] [ text "Memory" ]
                        , Table.th [ Table.numeric, css "width" "13%" ] [ text "MsgQ" ]
                        , Table.th [ Table.numeric, css "width" "24%" ] [ text "Current Function" ]
                        ]
                    ]
                , Table.tbody [] processes
                ]
            ]
        ]
        |> Views.Page.body


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (second * 5) Tick
