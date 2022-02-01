module Library.Dict.Nonempty exposing (..)

import Dict exposing (Dict)
import List.Extra as List


type alias NonemptyDict k v =
    ( ( k, v ), Dict k v )


toDict : NonemptyDict comparable v -> Dict comparable v
toDict ( ( headK, headV ), dictTail ) =
    Dict.insert headK headV dictTail


singleton : comparable -> v -> NonemptyDict comparable v
singleton headK headV =
    ( ( headK, headV ), Dict.empty )


head : NonemptyDict comparable v -> v
head ( ( _, headV ), _ ) =
    headV


insert : comparable -> v -> NonemptyDict comparable v -> NonemptyDict comparable v
insert newK newV ( ( headK, headV ), dictTail ) =
    if newK == headK then
        ( ( newK, newV ), dictTail )

    else
        ( ( headK, headV ), Dict.insert newK newV dictTail )


remove : comparable -> NonemptyDict comparable v -> Maybe (NonemptyDict comparable v)
remove k ( ( headK, headV ), dictTail ) =
    if k == headK then
        dictTail
            |> Dict.toList
            |> List.uncons
            |> (Maybe.map << Tuple.mapSecond) Dict.fromList

    else
        Just ( ( headK, headV ), Dict.remove k dictTail )


get : comparable -> NonemptyDict comparable v -> Maybe v
get k ( ( headK, headV ), dictTail ) =
    if k == headK then
        Just headV

    else
        Dict.get k dictTail


toList : NonemptyDict comparable v -> List ( comparable, v )
toList ( headPair, dictTail ) =
    headPair :: Dict.toList dictTail
