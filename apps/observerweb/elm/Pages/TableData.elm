module Pages.TableData exposing (..)

import Html exposing (..)
import Http
import Json.TablesData exposing (TableData, getTableData)
import Material
import Material.Button as Button
import Material.Icon as Icon
import Material.Options as Options exposing (css, onClick)
import Material.Table as Table
import Material.Typography as Typo
import Navigation
import Time exposing (Time, second)
import Views.Page


fetchdata : String -> Cmd Msg
fetchdata id =
    Http.send NewTableData (getTableData id)


type alias Model =
    { id : String
    , tableData : Maybe TableData
    , mdl : Material.Model
    }


init : String -> Model
init id =
    { id = id
    , tableData = Nothing
    , mdl = Material.model
    }


type Msg
    = NewTableData (Result Http.Error TableData)
    | BackMsg
    | Mdl (Material.Msg Msg)
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        NewTableData (Ok tableData) ->
            ( { model | tableData = Just tableData }, Cmd.none )

        NewTableData (Err _) ->
            ( model, Cmd.none )

        BackMsg ->
            ( model, Navigation.back 1 )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        Tick _ ->
            ( model, fetchdata model.id )


view : Model -> Html Msg
view model =
    let
        table_rows table_data =
            table_data
                |> List.indexedMap
                    (\i r ->
                        Table.tr []
                            [ Table.td
                                []
                                [ text <| toString i
                                ]
                            , Table.td
                                [ Table.numeric ]
                                [ pre [] [ text r ] ]
                            ]
                    )
    in
    case model.tableData of
        Nothing ->
            Options.styled p
                [ css "width" "100%"
                , css "margin" "16px"
                ]
                [ Options.styled p
                    [ Typo.headline ]
                    [ Button.render Mdl
                        [ 0, 0 ]
                        model.mdl
                        [ Button.icon, Button.ripple, onClick BackMsg ]
                        [ Icon.i "reply" ]
                    , text "Not found"
                    ]
                ]
                |> Views.Page.body

        Just tableData ->
            Options.styled p
                [ css "width" "100%"
                , css "margin" "16px"
                ]
                [ Options.styled p
                    [ Typo.headline ]
                    [ Button.render Mdl
                        [ 0, 0 ]
                        model.mdl
                        [ Button.icon, Button.ripple, onClick BackMsg ]
                        [ Icon.i "reply" ]
                    , text model.id
                    ]
                , Table.table
                    [ css "width" "100%", css "border" "0" ]
                    [ Table.thead []
                        [ Table.tr []
                            [ Table.th [ css "width" "20%" ] [ text "Index" ]
                            , Table.th [ Table.numeric, css "width" "80%" ] [ text "Data" ]
                            ]
                        ]
                    , Table.tbody [] (table_rows tableData.table_data)
                    ]
                ]
                |> Views.Page.body


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (second * 5) Tick
