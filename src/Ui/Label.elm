-- this is a wrapper for Element.Input label elements


module Ui.Label exposing
    ( Position(..)
    , default
    , defaultPosition
    , position
    , view
    )

import Element as E exposing (Element)
import Element.Input as Input


type Position
    = Above
    | Below
    | Left
    | Right
    | Hidden


defaultPosition : Position
defaultPosition =
    Above


type ViewConfig
    = ViewConfig
        { position : Position
        , text : String
        }


default : String -> ViewConfig
default text =
    ViewConfig
        { position = defaultPosition
        , text = text
        }


position : Position -> ViewConfig -> ViewConfig
position position_ (ViewConfig config) =
    ViewConfig { config | position = position_ }


hide : ViewConfig -> ViewConfig
hide (ViewConfig config) =
    ViewConfig { config | position = Hidden }


view : ViewConfig -> Input.Label msg
view (ViewConfig config) =
    let
        labelStyle : List (E.Attribute msg)
        labelStyle =
            [-- TODO style
            ]
    in
    case config.position of
        Above ->
            Input.labelAbove labelStyle <|
                E.text config.text

        Below ->
            Input.labelAbove labelStyle <|
                E.text config.text

        Left ->
            Input.labelAbove labelStyle <|
                E.text config.text

        Right ->
            Input.labelAbove labelStyle <|
                E.text config.text

        Hidden ->
            Input.labelAbove labelStyle <|
                E.text config.text
