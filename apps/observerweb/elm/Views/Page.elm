module Views.Page exposing (..)

import Html exposing (..)
import Material.Options as Options exposing (cs, css, styled)


body : Html a -> Html a
body contents =
    Options.div
        [ css "margin" "auto"
        , css "padding-left" "8%"
        , css "padding-right" "8%"
        , css "padding-bottom" "2cm"
        ]
        [ contents ]
