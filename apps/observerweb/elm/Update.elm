module Update exposing (update)

import Models exposing (Model)
import Msgs exposing (Msg)
import Routing exposing (parseLocation)


update msg model =
    case msg of
        Msgs.OnLocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
            ( { model | route = newRoute }, Cmd.none )
