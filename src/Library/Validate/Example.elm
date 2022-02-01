module Library.Validate.Example exposing (..)

import Library.Validate as Validate exposing (RecordValidation, Validation)


type alias User =
    { firstname : String
    , secondname : String
    , age : Int
    , bmi : Float
    , createdManually : Bool
    , thisFieldIsAlwaysInvalid : ()
    }


type alias Input =
    { firstname : String
    , secondname : String
    , age : Maybe String
    , heightMeter : Maybe String
    , weightKilogram : Maybe String
    }


userValidation : RecordValidation String Input User
userValidation =
    let
        floatValidation : Validation String (Maybe String) Float
        floatValidation =
            Validate.fromMaybe
                |> Validate.compose Validate.toFloat

        toBmi : ( Float, Float ) -> Float
        toBmi ( h, w ) =
            h / w ^ 2
    in
    Validate.record User
        |> Validate.field .firstname Validate.isNotEmptyString
        |> Validate.field .secondname Validate.isNotEmptyString
        |> Validate.field .age
            (Validate.fromMaybe
                |> Validate.compose Validate.toInt
                |> Validate.compose Validate.isNonNegativeNumber
                |> Validate.compose (Validate.intIsLowerThan 100)
            )
        |> Validate.field
            (\input -> ( input.heightMeter, input.weightKilogram ))
            (Validate.tuple ( floatValidation, floatValidation )
                |> Validate.compose (Validate.map toBmi)
                |> Validate.compose (Validate.floatIsLowerThan 40)
            )
        |> Validate.field identity (Validate.succeed True)
        |> Validate.field identity
            (Validate.fail "Sorry, this field is simply invalid")
        |> Validate.endRecord
