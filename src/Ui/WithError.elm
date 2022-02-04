module Ui.WithError exposing (error)

import Element as E exposing (Element)
import Html.Attributes as HtmlAttributes
import Maybe.Extra as Maybe


error : Maybe String -> Element msg -> Element msg
error maybeErrorText element =
    E.column []
        [ element
        , E.el
            [ -- This should be done with use of E.below
              -- but elm-ui-datepicker uses E.below as well for the calendar.
              -- And they overlap non-nicely.
              -- These are properties that E.below uses with change of z-indx from 20 ~> -1.
              E.htmlAttribute <| HtmlAttributes.style "position" "absolute"
            , E.htmlAttribute <| HtmlAttributes.style "bottom" "0"
            , E.htmlAttribute <| HtmlAttributes.style "left" "0"
            , E.htmlAttribute <| HtmlAttributes.style "height" "0"
            , E.htmlAttribute <| HtmlAttributes.style "width" "100%"
            , E.htmlAttribute <| HtmlAttributes.style "z-index" "-1"
            ]
          <|
            E.text (Maybe.unwrap "" ((++) "*") maybeErrorText)
        ]
