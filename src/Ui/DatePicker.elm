-- this is a wrapper for elm-ui-datepicker


module Ui.DatePicker exposing
    ( Model, init, getDate, initWithoutToday, setToday
    , Msg, update
    , default, label
    , view
    )

{-|


# Basic

@docs Model, init, getDate, initWithoutToday, setToday


# Update

@docs Msg, update


# ViewConfig builder

@docs default, label


# View

view

-}

import Date exposing (Date)
import DatePicker
import Either exposing (Either)
import Element as E exposing (Element)
import Element.Input as Input
import Return exposing (Return)
import Task
import Ui.Label as Label
import Ui.Placeholder as Placeholder



-- MODEL


type Model
    = {- Model is opaque and date has to be accesed
         with the exposed getDate function
      -}
      Model
        { state : DatePicker.Model
        , date : Either String Date
        }


init : Return Msg Model
init =
    Return.return
        (Model
            { state = DatePicker.init
            , date = Either.Left ""
            }
        )
        (Task.perform TodayRecieved Date.today)



---- when today not needed or want to set manually


initWithoutToday : Model
initWithoutToday =
    Model
        { state = DatePicker.init
        , date = Either.Left ""
        }


setToday : Date -> Model -> Model
setToday today (Model model) =
    Model
        { model
            | state =
                model.state
                    |> DatePicker.setToday today
        }



---- selected date accessor


getDate : Model -> Maybe Date
getDate (Model model) =
    model.date
        |> Either.toMaybe



-- UPDATE


type Msg
    = DatePickerMsg DatePicker.ChangeEvent
    | TodayRecieved Date


update : Msg -> Model -> Return Msg Model
update msg (Model model) =
    case msg of
        TodayRecieved today ->
            Model
                { model
                    | state =
                        model.state
                            |> DatePicker.setToday today
                }
                |> Return.singleton

        DatePickerMsg datePickerMsg ->
            case datePickerMsg of
                DatePicker.DateChanged date ->
                    Model
                        { model
                            | date = Either.Right date
                        }
                        |> Return.singleton

                DatePicker.TextChanged text ->
                    Model
                        { model
                            | date =
                                text
                                    |> Date.fromIsoString
                                    |> Either.fromResult
                        }
                        |> Return.singleton

                DatePicker.PickerChanged datePickerStateMsg ->
                    Model
                        { model
                            | state =
                                model.state
                                    |> DatePicker.update datePickerStateMsg
                        }
                        |> Return.singleton



-- VIEW


type ViewConfig
    = ViewConfig
        {- Opaque - will be created using config builder pattern.
           Allows for easy future feature adding and refactoring
           (don't have to update old occurences, just new ViewConfig
           transformators are added)
        -}
        { label : String }


default :
    { -- label is always required for accessibility
      label : String
    }
    -> ViewConfig
default required =
    ViewConfig
        { label = required.label }


label : String -> ViewConfig -> ViewConfig
label label_ (ViewConfig config) =
    ViewConfig
        { config | label = label_ }


view : ViewConfig -> Model -> Element Msg
view (ViewConfig config) (Model model) =
    DatePicker.input []
        { onChange = DatePickerMsg
        , selected = getDate (Model model)
        , text = Either.unpack identity Date.toIsoString model.date
        , label = Label.view (Label.default config.label)
        , placeholder = Just <| Placeholder.view (Placeholder.default "YYYY-MM-DD")
        , model = model.state
        , settings = DatePicker.defaultSettings
        }
