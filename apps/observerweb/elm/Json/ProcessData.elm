module Json.ProcessData exposing (Process, Processes, getProcesses)

import Http
import Json.Decode exposing (Decoder, list, string)
import Json.Decode.Pipeline exposing (decode, required)


getProcesses : Http.Request Processes
getProcesses =
    Http.post "/info" (Http.stringBody "text/plain" "action=get_pro&type=all") processesDecoder


type alias Process =
    { current_function : String
    , memory : String
    , message_queue_len : String
    , registered_name : String
    , initial_call : String
    , pid : String
    , reductions : String
    , status : String
    , group_leader : String
    , total_heap_size : String
    , stack_size : String
    , links : List String
    , ancestors : List String
    , monitors : List String
    , garbage_collection : GarbageCollection
    }


type alias GarbageCollection =
    { min_heap_size : String
    , fullsweep_after : String
    }


type alias Processes =
    { processes : List Process
    }


gcDecoder : Decoder GarbageCollection
gcDecoder =
    decode GarbageCollection
        |> required "min_heap_size" string
        |> required "fullsweep_after" string


processDecoder : Decoder Process
processDecoder =
    decode Process
        |> required "current_function" string
        |> required "memory" string
        |> required "message_queue_len" string
        |> required "registered_name" string
        |> required "initial_call" string
        |> required "pid" string
        |> required "reductions" string
        |> required "status" string
        |> required "group_leader" string
        |> required "total_heap_size" string
        |> required "stack_size" string
        |> required "links" (list string)
        |> required "ancestors" (list string)
        |> required "monitors" (list string)
        |> required "garbage_collection" gcDecoder


processesDecoder : Decoder Processes
processesDecoder =
    decode Processes
        |> required "process_table" (list processDecoder)
