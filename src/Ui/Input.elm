-- this is a wrapper for Element.Input.text


module Ui.Input exposing
    ( default
    , hideLabel
    , labelPosition
    , view
    )

import Element as E exposing (Element)
import Element.Input as Input
import Ui.Label as Label
import Ui.Placeholder as Placeholder


type ViewConfig msg
    = ViewConfig
        { --required
          onChange : String -> msg
        , text : String
        , label : String

        -- other
        , labelPosition : Label.Position
        , placeholder : Maybe String
        }


default :
    { onChange : String -> msg
    , text : String

    -- label is always required for accessibility
    , label : String
    }
    -> ViewConfig msg
default required =
    ViewConfig
        { onChange = required.onChange
        , text = required.text
        , label = required.label
        , labelPosition = Label.defaultPosition
        , placeholder = Nothing
        }


labelPosition : Label.Position -> ViewConfig msg -> ViewConfig msg
labelPosition position (ViewConfig config) =
    ViewConfig { config | labelPosition = position }


hideLabel : ViewConfig msg -> ViewConfig msg
hideLabel config =
    config
        |> labelPosition Label.Hidden


view : ViewConfig msg -> Element msg
view (ViewConfig config) =
    Input.text
        [-- TODO
        ]
        { onChange = config.onChange
        , text = config.text
        , placeholder =
            Maybe.map
                (\placeholder ->
                    Placeholder.view (Placeholder.default placeholder)
                )
                config.placeholder
        , label =
            Label.view (Label.default config.label)
        }
