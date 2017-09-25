module Pages.Ports exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)
import Http
import Json.PortsData exposing (Ports, getPorts)
import Material
import Material.Options as Options exposing (css, onClick)
import Material.Table as Table
import Routing
import Time exposing (Time, second)
import Views.Page


fetchdata : Cmd Msg
fetchdata =
    Http.send NewPorts getPorts


type alias Model =
    { ports : Maybe Ports
    , mdl : Material.Model
    }


model : Model
model =
    { ports = Nothing
    , mdl = Material.model
    }


type Msg
    = NewPorts (Result Http.Error Ports)
    | Mdl (Material.Msg Msg)
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        NewPorts (Ok ports) ->
            ( { model | ports = Just ports }, Cmd.none )

        NewPorts (Err e) ->
            ( model, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        Tick _ ->
            ( model, fetchdata )


view : Model -> Html Msg
view model =
    let
        ports =
            case model.ports of
                Nothing ->
                    [ Table.tr [] [] ]

                Just ports ->
                    ports.ports
                        |> List.map
                            (\item ->
                                Table.tr []
                                    [ Table.td
                                        []
                                        [ text <| toString item.id ]
                                    , Table.td
                                        [ Table.numeric ]
                                        [ text item.name ]
                                    , Table.td
                                        [ Table.numeric ]
                                        [ a [ Routing.processPath item.connected |> href ] [ text item.connected ] ]
                                    , Table.td
                                        []
                                        [ text <| toString item.input ]
                                    , Table.td
                                        []
                                        [ text <| toString item.output ]
                                    , Table.td
                                        [ Table.numeric ]
                                        (List.map (\l -> a [ Routing.processPath l |> href ] [ text l ]) item.links)
                                    ]
                            )
    in
    Table.table [ css "width" "100%", css "border" "0" ]
        [ Table.thead []
            [ Table.tr []
                [ Table.th [ css "width" "10%" ] [ text "Id" ]
                , Table.th [ Table.numeric, css "width" "27%" ] [ text "Name" ]
                , Table.th [ Table.numeric, css "width" "10%" ] [ text "Owner" ]
                , Table.th [ css "width" "10%" ] [ text "Input" ]
                , Table.th [ css "width" "10%" ] [ text "Output" ]
                , Table.th [ Table.numeric, css "width" "33%" ] [ text "Links" ]
                ]
            ]
        , Table.tbody [] ports
        ]
        |> Views.Page.body


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (second * 5) Tick
