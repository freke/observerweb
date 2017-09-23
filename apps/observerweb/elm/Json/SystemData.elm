module Json.SystemData exposing (SystemInfo, Value, getSystem)

import Http
import Json.Decode exposing (Decoder, list, string)
import Json.Decode.Pipeline exposing (decode, required)


getSystem : Http.Request SystemInfo
getSystem =
    Http.post "/info" (Http.stringBody "text/plain" "action=get_sys") systemInfoDecoder


type alias Value =
    { name : String
    , value : String
    }


type alias SystemInfo =
    { cpu : Timestamped
    , memory : Timestamped
    , statistics : Timestamped
    , system : Timestamped
    }


type alias Timestamped =
    { time : String
    , data : List Value
    }


valueDecoder : Decoder Value
valueDecoder =
    decode Value
        |> required "name" string
        |> required "value" string


timestampedDecoder : Decoder Timestamped
timestampedDecoder =
    decode Timestamped
        |> required "time" string
        |> required "data" (list valueDecoder)


systemInfoDecoder : Decoder SystemInfo
systemInfoDecoder =
    decode SystemInfo
        |> required "cpu" timestampedDecoder
        |> required "memory" timestampedDecoder
        |> required "statistics" timestampedDecoder
        |> required "system" timestampedDecoder
