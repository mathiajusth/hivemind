-- this is a wrapper for elm-ui-datepicker


module Library.DatePicker exposing (Model, Msg, getDate, init, update)

import Date exposing (Date)
import DatePicker
import Either exposing (Either)
import Return exposing (Return)
import Task


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


getDate : Model -> Maybe Date
getDate (Model model) =
    model.date
        |> Either.toMaybe


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
