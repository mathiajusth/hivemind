module Data.Product exposing (Id)

import Library.Id
import Time


type ProductIdTag
    = ProductIdTag


type alias Id =
    Library.Id.Id ProductIdTag


type alias Query =
    { start : Time.Posix
    , end : Time.Posix
    , limit : Int
    , minNumberReviews : Int
    }
