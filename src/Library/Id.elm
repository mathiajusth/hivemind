module Library.Id exposing
    ( Id
    , decoder
    , encode
    , toString
    )

import Json.Decode as Decode
import Json.Encode as Encode


type Id tag
    = {- We use phantom type to distinghuis IDs' of different entities
         so we cannot mix e.g. productID with reviewerID.

         You can define an Id for a particular entity this way

              type EntityIdTag =
                EntityIdTag

              type alias Id =
                Library.Id.Id EntityIdTag

         The EntityIdTag will be infered automatically by type inference during decoding
      -}
      Id String


toString : Id tag -> String
toString (Id string) =
    {- Sometimes IDs need to be shown to the users. -}
    string


decoder : Decode.Decoder (Id tag)
decoder =
    Decode.string
        |> Decode.map Id


encode : Id tag -> Encode.Value
encode (Id string) =
    Encode.string string
