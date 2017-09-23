module Pages.Applications exposing (..)

import Html exposing (..)
import Markdown
import Views.Page


type alias Model =
    {}


model : Model
model =
    {}


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Noop ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    """
Applications
  """
        |> Markdown.toHtml []
        |> Views.Page.body


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
