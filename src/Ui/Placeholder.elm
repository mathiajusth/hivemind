-- this is a wrapper for  placeholder used in forms


module Ui.Placeholder exposing
    ( default
    , view
    )

import Element as E exposing (Element)
import Element.Input as Input


type ViewConfig
    = ViewConfig { text : String }


default : String -> ViewConfig
default text =
    ViewConfig
        { text = text }


view : ViewConfig -> Input.Placeholder msg
view (ViewConfig config) =
    Input.placeholder
        [-- TODO
        ]
    <|
        E.text config.text
