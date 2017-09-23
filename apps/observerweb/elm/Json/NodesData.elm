module Json.NodesData exposing (ConnectNodeResponse, Nodes, connectNodeResponseDecoder, nodesDecoder)

import Json.Decode exposing (Decoder, bool, list, nullable, string)
import Json.Decode.Pipeline exposing (decode, optional, required)


type alias Nodes =
    { nodes : List String
    }


nodesDecoder : Decoder Nodes
nodesDecoder =
    decode Nodes
        |> required "nodes" (list string)


type alias ConnectNodeResponse =
    { node : String
    , connected : Bool
    , message : Maybe String
    }


connectNodeResponseDecoder : Decoder ConnectNodeResponse
connectNodeResponseDecoder =
    decode ConnectNodeResponse
        |> required "node" string
        |> required "connected" bool
        |> optional "message" (nullable string) Nothing
