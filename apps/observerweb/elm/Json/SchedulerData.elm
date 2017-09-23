module Json.SchedulerData exposing (Scheduler, ShedulersUtilization, getSchedulers, shedulersUtilization)

import Dict exposing (Dict, get)
import Http
import Json.Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (decode, required)


type alias Data =
    { schedulerid : Int
    , activetime : Int
    , totaltime : Int
    }


type alias Timestamped =
    { time : String
    , data : List Data
    }


type alias Scheduler =
    { scheduler : Timestamped }


type alias ShedulersUtilization =
    { schedulerid : Int
    , utilization : Int
    }


getSchedulers : Http.Request Scheduler
getSchedulers =
    Http.post "/info" (Http.stringBody "text/plain" "action=get_perf&type=scheduler") schedulerDecoder


dataDecoder : Decoder Data
dataDecoder =
    decode Data
        |> required "schedulerid" int
        |> required "activetime" int
        |> required "totaltime" int


timestampedDecoder : Decoder Timestamped
timestampedDecoder =
    decode Timestamped
        |> required "time" string
        |> required "data" (list dataDecoder)


schedulerDecoder : Decoder Scheduler
schedulerDecoder =
    decode Scheduler
        |> required "scheduler" timestampedDecoder


shedulersUtilization : Int -> ( Int, Int ) -> Dict Int { activetime : Int, totaltime : Int } -> ShedulersUtilization
shedulersUtilization schedulerid ( activetime, totaltime ) last_schedulers =
    let
        last_values =
            get schedulerid last_schedulers
    in
    case last_values of
        Just last ->
            { schedulerid = schedulerid
            , utilization = floor (100 * toFloat (activetime - last.activetime) / toFloat (totaltime - last.totaltime))
            }

        Nothing ->
            { schedulerid = schedulerid
            , utilization = 0
            }
