module Data.Product exposing (Id)

import Library.Id
import Time


type ProductIdTag
    = ProductIdTag


type alias Id =
    Library.Id.Id ProductIdTag
