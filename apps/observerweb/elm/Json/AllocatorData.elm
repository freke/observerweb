module Json.AllocatorData exposing (Allocator, AllocatorData, getAllocator)

import Http
import Json.Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (decode, required)


getAllocator : Http.Request Allocator
getAllocator =
    Http.post "/info" (Http.stringBody "text/plain" "action=get_malloc") allocatorDecoder


type alias Timestamped =
    { time : String
    , data : List AllocatorData
    }


type alias Allocator =
    { allocator : Timestamped }


type alias AllocatorData =
    { name : String
    , bs : Int
    , cs : Int
    }


allocatorDecoder : Decoder Allocator
allocatorDecoder =
    decode Allocator
        |> required "allocator" timestampedDecoder


timestampedDecoder : Decoder Timestamped
timestampedDecoder =
    decode Timestamped
        |> required "time" string
        |> required "data" (list dataDecoder)


dataDecoder : Decoder AllocatorData
dataDecoder =
    decode AllocatorData
        |> required "name" string
        |> required "bs" int
        |> required "cs" int
