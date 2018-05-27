module Json.WheelEvent exposing (WheelEvent, wheelEvent)

import Json.Decode exposing (Decoder, bool, float, int)
import Json.Decode.Pipeline exposing (decode, required)


type alias WheelEvent =
    { screenX : Int
    , screenY : Int
    , clientX : Int
    , clientY : Int
    , ctrlKey : Bool
    , shiftKey : Bool
    , altKey : Bool
    , metaKey : Bool
    , button : Int
    , deltaX : Float
    , deltaY : Float
    , deltaZ : Float
    , deltaMode : Int
    }


wheelEvent : Decoder WheelEvent
wheelEvent =
    decode WheelEvent
        |> required "screenX" int
        |> required "screenY" int
        |> required "clientX" int
        |> required "clientY" int
        |> required "ctrlKey" bool
        |> required "shiftKey" bool
        |> required "altKey" bool
        |> required "metaKey" bool
        |> required "button" int
        |> required "deltaX" float
        |> required "deltaY" float
        |> required "deltaZ" float
        |> required "deltaMode" int
