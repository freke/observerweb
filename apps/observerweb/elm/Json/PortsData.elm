module Json.PortsData exposing (Ports, getPorts)

import Http
import Json.Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (decode, required)


getPorts : Http.Request Ports
getPorts =
    Http.post "/info" (Http.stringBody "text/plain" "action=get_ports") portsDecoder


type alias Port =
    { output : Int
    , os_pid : String
    , name : String
    , links : List String
    , input : Int
    , id : Int
    , connected : String
    }


type alias Ports =
    { ports : List Port
    }


portDecoder : Decoder Port
portDecoder =
    decode Port
        |> required "output" int
        |> required "os_pid" string
        |> required "name" string
        |> required "links" (list string)
        |> required "input" int
        |> required "id" int
        |> required "connected" string


portsDecoder : Decoder Ports
portsDecoder =
    decode Ports
        |> required "port_table" (list portDecoder)
