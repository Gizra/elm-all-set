module EverySetList
    exposing
        ( EverySetList
        , diff
        , empty
        , filter
        , foldl
        , foldr
        , fromList
        , insert
        , intersect
        , isEmpty
        , map
        , member
        , partition
        , remove
        , singleton
        , size
        , toList
        , union
        )

{-| A set of unique values. The values can be any type, as the implementation is
based on [EveryDict](http://package.elm-lang.org/packages/eeue56/elm-all-dict/latest)


# Sets

@docs EverySet


# Build

@docs empty, singleton, insert, remove


# Query

@docs isEmpty, member, size


# Combine

@docs union, intersect, diff


# Lists

@docs toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition

-}

import EveryDictList exposing (EveryDictList)


{-| Represents a set of unique values. So `(Set Int)` is a set of integers and
`(Set String)` is a set of strings.
-}
type EverySetList a
    = EverySetList (EveryDictList a ())


{-| Create an empty set.
-}
empty : EverySetList a
empty =
    EverySetList EveryDictList.empty


{-| Create a set with one value.
-}
singleton : a -> EverySetList a
singleton k =
    EverySetList <| EveryDictList.singleton k ()


{-| Insert a value into a set.
-}
insert : a -> EverySetList a -> EverySetList a
insert k (EverySetList d) =
    EverySetList <| EveryDictList.insert k () d


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : a -> EverySetList a -> EverySetList a
remove k (EverySetList d) =
    EverySetList <| EveryDictList.remove k d


{-| Determine if a set is empty.
-}
isEmpty : EverySetList a -> Bool
isEmpty (EverySetList d) =
    EveryDictList.isEmpty d


{-| Determine if a value is in a set.
-}
member : a -> EverySetList a -> Bool
member k (EverySetList d) =
    EveryDictList.member k d


{-| Determine the number of elements in a set.
-}
size : EverySetList a -> Int
size (EverySetList d) =
    EveryDictList.size d


{-| Get the union of two sets. Keep all values.
-}
union : EverySetList a -> EverySetList a -> EverySetList a
union (EverySetList d1) (EverySetList d2) =
    EverySetList <| EveryDictList.union d1 d2


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : EverySetList a -> EverySetList a -> EverySetList a
intersect (EverySetList d1) (EverySetList d2) =
    EverySetList <| EveryDictList.intersect d1 d2


{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set.
-}
diff : EverySetList a -> EverySetList a -> EverySetList a
diff (EverySetList d1) (EverySetList d2) =
    EverySetList <| EveryDictList.diff d1 d2


{-| Convert a set into a list, sorted from lowest to highest.
-}
toList : EverySetList a -> List a
toList (EverySetList d) =
    EveryDictList.keys d


{-| Convert a list into a set, removing any duplicates.
-}
fromList : List a -> EverySetList a
fromList xs =
    List.foldl insert empty xs


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : (a -> b -> b) -> b -> EverySetList a -> b
foldl f b (EverySetList d) =
    EveryDictList.foldl (\k _ b -> f k b) b d


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : (a -> b -> b) -> b -> EverySetList a -> b
foldr f b (EverySetList d) =
    EveryDictList.foldr (\k _ b -> f k b) b d


{-| Map a function onto a set, creating a new set with no duplicates.
-}
map : (a -> a2) -> EverySetList a -> EverySetList a2
map f s =
    fromList (List.map f (toList s))


{-| Create a new set consisting only of elements which satisfy a predicate.
-}
filter : (a -> Bool) -> EverySetList a -> EverySetList a
filter p (EverySetList d) =
    EverySetList <| EveryDictList.filter (\k _ -> p k) d


{-| Create two new sets; the first consisting of elements which satisfy a
predicate, the second consisting of elements which do not.
-}
partition : (a -> Bool) -> EverySetList a -> ( EverySetList a, EverySetList a )
partition p (EverySetList d) =
    let
        ( p1, p2 ) =
            EveryDictList.partition (\k _ -> p k) d
    in
    ( EverySetList p1, EverySetList p2 )
