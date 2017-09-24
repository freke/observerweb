module Json.TablesData exposing (Tables, getTables)

import Http
import Json.Decode exposing (Decoder, bool, list, string)
import Json.Decode.Pipeline exposing (decode, required)


getTables : Http.Request Tables
getTables =
    Http.post "/info" (Http.stringBody "text/plain" "action=get_tables") tablesDecoder


type alias Table =
    { name : String
    , table_type : String
    , protection : String
    , owner : String
    , table_size : String
    , memory : String
    , compressed : Bool
    }


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
        |> required "size" string
        |> required "memory" string
        |> required "compressed" bool


tablesDecoder : Decoder Tables
tablesDecoder =
    decode Tables
        |> required "ets_table" (list tableDecoder)
