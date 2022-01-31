module Data.Review exposing (Review, decoder)

import Data.Product as Product
import Data.Rating as Rating exposing (Rating)
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Library.Id as Id
import Time


type alias Review =
    { productID : Product.Id
    , time : Time.Posix
    , rating : Rating
    }


decoder : Decode.Decoder Review
decoder =
    Decode.succeed Review
        |> Decode.andMap (Decode.field "asin" Id.decoder)
        |> Decode.andMap (Decode.field "unixReviewTime" Decode.datetime)
        |> Decode.andMap
            (Decode.field "rating" Rating.decoder
             -- We are currenly failing if rating is outsied of 1 to 5 range
             -- This can be changed to fail gracefully i.e. just ignore that review
            )
