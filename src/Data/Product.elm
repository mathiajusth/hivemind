module Data.Product exposing (Id)

import Library.Id


type ProductIdTag
    = ProductIdTag


type alias Id =
    Library.Id.Id ProductIdTag
