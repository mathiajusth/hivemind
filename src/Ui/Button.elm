module Ui.Button exposing
    ( default
    , onClick
    , view
    )

import Element as E exposing (Element)
import Element.Input as Input


type ViewConfig msg
    = ViewConfig
        { --required
          text : String

        -- other
        , onClick : Maybe msg
        }


default : String -> ViewConfig msg
default text =
    ViewConfig
        { onClick = Nothing
        , text = text
        }


onClick : msg -> ViewConfig msgOld -> ViewConfig msg
onClick onClickMsg (ViewConfig config) =
    ViewConfig
        { text = config.text
        , onClick = Just onClickMsg
        }


view : ViewConfig msg -> Element msg
view (ViewConfig config) =
    Input.button
        [-- TODO style
        ]
        { onPress = config.onClick, label = E.text config.text }
