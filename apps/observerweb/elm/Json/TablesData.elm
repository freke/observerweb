module Json.TablesData exposing (TableData, Tables, getTableData, getTables)

import Http
import Json.Decode exposing (Decoder, bool, int, list, string)
import Json.Decode.Pipeline exposing (decode, required)


getTables : Http.Request Tables
getTables =
    Http.post "/info" (Http.stringBody "text/plain" "action=get_tables") tablesDecoder


getTableData : String -> Http.Request TableData
getTableData table =
    Http.post "/info" (Http.stringBody "text/plain" ("action=get_tables&table=" ++ table)) tableDataDecoder


type alias Table =
    { name : String
    , table_type : String
    , protection : String
    , owner : String
    , table_size : Int
    , memory : Int
    , compressed : Bool
    }


type alias TableData =
    { table_data : List String }


type alias Tables =
    { tables : List Table
    }


tableDecoder : Decoder Table
tableDecoder =
    decode Table
        |> required "name" string
        |> required "type" string
        |> required "protection" string
        |> required "owner" string
        |> required "size" int
        |> required "memory" int
        |> required "compressed" bool


tablesDecoder : Decoder Tables
tablesDecoder =
    decode Tables
        |> required "ets_table" (list tableDecoder)


tableDataDecoder : Decoder TableData
tableDataDecoder =
    decode TableData
        |> required "ets_table_data" (list string)
