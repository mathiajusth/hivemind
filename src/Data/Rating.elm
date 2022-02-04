module Data.Rating exposing (Rating, average, decoder)

import Json.Decode as Decode
import Json.Decode.Extra as Decode
import List.Nonempty as NonemptyList


type Rating
    = Stars Int


average : NonemptyList.Nonempty Rating -> Float
average ratings =
    {- Uses nonempty list to ensure that average is not Infinity  by dividing with zero
       as semantically the best rated thing isn't the one without any ratings
    -}
    let
        totalStarsCount =
            ratings
                |> NonemptyList.toList
                |> List.map (\(Stars int) -> int)
                |> List.sum
                |> toFloat

        ratingsCount =
            ratings
                |> NonemptyList.length
                |> toFloat
    in
    totalStarsCount / ratingsCount


decoder : Decode.Decoder Rating
decoder =
    let
        fromInt : Int -> Maybe Rating
        fromInt int =
            if 1 <= int && int <= 5 then
                Just <| Stars int

            else
                Nothing
    in
    Decode.int
        |> Decode.andThen
            (fromInt
                >> Decode.fromMaybe "Rating has to be 1 to 5"
            )
