module Json.IoData exposing (Io, getIo)

import Http
import Json.Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (decode, required)


type alias Io =
    { input : Int
    , output : Int
    , time : String
    }


getIo : Http.Request Io
getIo =
    Http.post "/info" (Http.stringBody "text/plain" "action=get_perf&type=io") ioDecoder


ioDecoder : Decoder Io
ioDecoder =
    decode Io
        |> required "input" int
        |> required "output" int
        |> required "time" string
