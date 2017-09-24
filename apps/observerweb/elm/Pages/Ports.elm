module Pages.Ports exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)
import Http
import Json.PortsData exposing (Ports, getPorts)
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
                                        [ text item.id ]
                                    , Table.td
                                        [ Table.numeric ]
                                        [ text item.name ]
                                    , Table.td
                                        [ Table.numeric ]
                                        [ a [ Routing.processPath item.connected |> href ] [ text item.connected ] ]
                                    , Table.td
                                        []
                                        [ text item.input ]
                                    , Table.td
                                        []
                                        [ text item.output ]
                                    , Table.td
                                        [ Table.numeric ]
                                        (List.map (\l -> a [ Routing.processPath l |> href ] [ text l ]) item.links)
                                    ]
                            )
    in
    Card.view
        [ Elevation.e2, css "width" "100%", css "margin" "16px" ]
        [ Card.title [] [ Card.head [] [ text "Ports Table" ] ]
        , Card.text [ css "width" "100%", css "padding" "0" ]
            [ Table.table [ css "width" "100%", css "border" "0" ]
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
            ]
        ]
        |> Views.Page.body


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (second * 5) Tick
