module Library.List.Extra exposing (..)

import Basics.Extra as Basics
import List.Extra as List
import List.Nonempty as NonemptyList


type alias Grouped a =
    List (NonemptyList.Nonempty a)


groupEqualsBy : (a -> b) -> List a -> Grouped a
groupEqualsBy check =
    List.gatherEqualsBy check
        >> List.map (Basics.uncurry NonemptyList.Nonempty)


mapGroupMembers : (a -> b) -> Grouped a -> Grouped b
mapGroupMembers =
    List.map << NonemptyList.map


filterGroupMembers : (a -> Bool) -> Grouped a -> Grouped a
filterGroupMembers check =
    List.filterMap
        (\group ->
            group
                |> NonemptyList.toList
                |> List.filter check
                |> NonemptyList.fromList
        )
