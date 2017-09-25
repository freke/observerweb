module Pages.TableViewer exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)
import Http
import Json.TablesData exposing (Tables, getTables)
import Material
import Material.Icon as Icon
import Material.Options as Options exposing (css, onClick)
import Material.Table as Table
import Routing
import Time exposing (Time, second)
import Views.Page


fetchdata : Cmd Msg
fetchdata =
    Http.send NewTables getTables


type alias Model =
    { tables : Maybe Tables
    , mdl : Material.Model
    }


model : Model
model =
    { tables = Nothing
    , mdl = Material.model
    }


type Msg
    = NewTables (Result Http.Error Tables)
    | Mdl (Material.Msg Msg)
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        NewTables (Ok tables) ->
            ( { model | tables = Just tables }, Cmd.none )

        NewTables (Err e) ->
            ( model, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        Tick _ ->
            ( model, fetchdata )


view : Model -> Html Msg
view model =
    let
        tables =
            case model.tables of
                Nothing ->
                    [ Table.tr [] [] ]

                Just tables ->
                    tables.tables
                        |> List.map
                            (\item ->
                                Table.tr []
                                    [ Table.td
                                        [ Table.numeric ]
                                        [ if (item.protection /= "private") && (item.table_size > 0) then
                                            a [ Routing.tablePath item.name |> href ] [ text item.name ]
                                          else
                                            text item.name
                                        ]
                                    , Table.td
                                        [ Table.numeric ]
                                        [ text item.table_type ]
                                    , Table.td
                                        [ Table.numeric ]
                                        [ text item.protection ]
                                    , Table.td
                                        [ Table.numeric ]
                                        [ a [ Routing.processPath item.owner |> href ] [ text item.owner ] ]
                                    , Table.td
                                        []
                                        [ text <| toString item.table_size ]
                                    , Table.td
                                        []
                                        [ text <| toString item.memory ]
                                    , Table.td
                                        [ Table.numeric ]
                                        [ if item.compressed then
                                            Icon.view "done" [ css "width" "40px" ]
                                          else
                                            text ""
                                        ]
                                    ]
                            )
    in
    Table.table [ css "width" "100%", css "border" "0" ]
        [ Table.thead []
            [ Table.tr []
                [ Table.th [ Table.numeric, css "width" "28%" ] [ text "Name" ]
                , Table.th [ Table.numeric, css "width" "12%" ] [ text "Type" ]
                , Table.th [ Table.numeric, css "width" "12%" ] [ text "Protection" ]
                , Table.th [ Table.numeric, css "width" "12%" ] [ text "Owner" ]
                , Table.th [ css "width" "12%" ] [ text "Size" ]
                , Table.th [ css "width" "12%" ] [ text "Memory" ]
                , Table.th [ Table.numeric, css "width" "12%" ] [ text "Compressed" ]
                ]
            ]
        , Table.tbody [] tables
        ]
        |> Views.Page.body


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (second * 5) Tick
