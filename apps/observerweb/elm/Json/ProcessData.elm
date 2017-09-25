module Json.ProcessData exposing (Process, Processes, getProcesses)

import Http
import Json.Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (decode, required)


getProcesses : Http.Request Processes
getProcesses =
    Http.post "/info" (Http.stringBody "text/plain" "action=get_pro&type=all") processesDecoder


type alias Process =
    { current_function : String
    , memory : Int
    , message_queue_len : Int
    , registered_name : String
    , initial_call : String
    , pid : String
    , reductions : Int
    , status : String
    , group_leader : String
    , total_heap_size : Int
    , stack_size : Int
    , links : List String
    , ancestors : List String
    , monitors : List String
    , garbage_collection : GarbageCollection
    }


type alias GarbageCollection =
    { min_heap_size : Int
    , fullsweep_after : Int
    }


type alias Processes =
    { processes : List Process
    }


gcDecoder : Decoder GarbageCollection
gcDecoder =
    decode GarbageCollection
        |> required "min_heap_size" int
        |> required "fullsweep_after" int


processDecoder : Decoder Process
processDecoder =
    decode Process
        |> required "current_function" string
        |> required "memory" int
        |> required "message_queue_len" int
        |> required "registered_name" string
        |> required "initial_call" string
        |> required "pid" string
        |> required "reductions" int
        |> required "status" string
        |> required "group_leader" string
        |> required "total_heap_size" int
        |> required "stack_size" int
        |> required "links" (list string)
        |> required "ancestors" (list string)
        |> required "monitors" (list string)
        |> required "garbage_collection" gcDecoder


processesDecoder : Decoder Processes
processesDecoder =
    decode Processes
        |> required "process_table" (list processDecoder)
