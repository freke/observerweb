module Json.MemoryData exposing (Memory, getMemory)

import Http
import Json.Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (decode, required)


type alias Memory =
    { total : Int
    , processes : Int
    , atom : Int
    , binary : Int
    , code : Int
    , ets : Int
    , time : String
    }


getMemory : Http.Request Memory
getMemory =
    Http.post "/info" (Http.stringBody "text/plain" "action=get_perf&type=memory") memoryDecoder


memoryDecoder : Decoder Memory
memoryDecoder =
    decode Memory
        |> required "total" int
        |> required "processes" int
        |> required "atom" int
        |> required "binary" int
        |> required "code" int
        |> required "ets" int
        |> required "time" string
