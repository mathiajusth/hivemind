module Data.Query exposing (Form)

import Date
import Library.DatePicker as DatePicker
import Library.Time.Extra as Time
import Library.Validate as Validate exposing (Validation)
import Time
import Time.Extra as Time


type alias Form =
    { start : DatePicker.Model
    , end : DatePicker.Model
    , limit : String
    , minNumberReviews : String
    }


type alias Query =
    { start : Time.Posix
    , end : Time.Posix
    , limit : Int
    , minNumberReviews : Int
    }


validate : Validate.RecordValidation String Form Query
validate =
    Validate.record Query
        |> Validate.field .start
            (Validate.map DatePicker.getDate
                |> Validate.compose Validate.fromMaybe
                |> Validate.compose
                    (Validate.map (Time.fromDate Time.StartOfDay Time.utc))
            )
        |> Validate.field .end
            (Validate.map DatePicker.getDate
                |> Validate.compose Validate.fromMaybe
                |> Validate.compose
                    (Validate.map (Time.fromDate Time.EndOfDay Time.utc))
            )
        |> Validate.field .limit Validate.toInt
        |> Validate.field .minNumberReviews Validate.toInt
        |> Validate.endRecord
