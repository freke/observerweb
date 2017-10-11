module Json.AppsData exposing (AppInfo, Apps, Children, ProcessInfoApp, getAppInfo, getApps, unwrapChildren)

import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (decode, required)


type alias Apps =
    { apps : List App }


type alias App =
    { vsn : String
    , name : String
    , descr : String
    }


type alias Monitor =
    { process_type : String
    , pid : String
    }


type alias AppInfo =
    { app : ProcessInfoApp
    }


type alias ProcessInfoApp =
    { pid : String
    , name : String
    , monitors : List Monitor
    , children : Children
    }


type Children
    = Children (List ProcessInfoApp)


unwrapChildren : Children -> List ProcessInfoApp
unwrapChildren (Children children) =
    children


getApps : Http.Request Apps
getApps =
    Http.post "/info" (Http.stringBody "text/plain" "action=get_apps") appsDecoder


getAppInfo : String -> Http.Request AppInfo
getAppInfo app =
    Http.post "/info" (Http.stringBody "text/plain" ("action=get_apps&app=" ++ app)) decodeAppInfo


appsDecoder : Decoder Apps
appsDecoder =
    decode Apps
        |> required "apps" (list appDecoder)


appDecoder : Decoder App
appDecoder =
    decode App
        |> required "vsn" string
        |> required "name" string
        |> required "descr" string


decodeMonitor : Decoder Monitor
decodeMonitor =
    decode Monitor
        |> required "type" string
        |> required "pid" string


decodeAppInfo : Decoder AppInfo
decodeAppInfo =
    decode AppInfo
        |> required "app" decodeProcessInfoApp


decodeProcessInfoApp : Decoder ProcessInfoApp
decodeProcessInfoApp =
    Json.Decode.Pipeline.decode ProcessInfoApp
        |> required "pid" string
        |> required "name" string
        |> required "monitors" (list decodeMonitor)
        |> required "children" decodeChildren


decodeChildren : Decoder Children
decodeChildren =
    Decode.map Children (Decode.list (Decode.lazy (\_ -> decodeProcessInfoApp)))
