module EverySet
    exposing
        ( EverySet
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

import EveryDict exposing (EveryDict)


{-| Represents a set of unique values. So `(Set Int)` is a set of integers and
`(Set String)` is a set of strings.
-}
type EverySet a
    = EverySet (EveryDict a ())


{-| Create an empty set.
-}
empty : EverySet a
empty =
    EverySet EveryDict.empty


{-| Create a set with one value.
-}
singleton : a -> EverySet a
singleton k =
    EverySet <| EveryDict.singleton k ()


{-| Insert a value into a set.
-}
insert : a -> EverySet a -> EverySet a
insert k (EverySet d) =
    EverySet <| EveryDict.insert k () d


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : a -> EverySet a -> EverySet a
remove k (EverySet d) =
    EverySet <| EveryDict.remove k d


{-| Determine if a set is empty.
-}
isEmpty : EverySet a -> Bool
isEmpty (EverySet d) =
    EveryDict.isEmpty d


{-| Determine if a value is in a set.
-}
member : a -> EverySet a -> Bool
member k (EverySet d) =
    EveryDict.member k d


{-| Determine the number of elements in a set.
-}
size : EverySet a -> Int
size (EverySet d) =
    EveryDict.size d


{-| Get the union of two sets. Keep all values.
-}
union : EverySet a -> EverySet a -> EverySet a
union (EverySet d1) (EverySet d2) =
    EverySet <| EveryDict.union d1 d2


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : EverySet a -> EverySet a -> EverySet a
intersect (EverySet d1) (EverySet d2) =
    EverySet <| EveryDict.intersect d1 d2


{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set.
-}
diff : EverySet a -> EverySet a -> EverySet a
diff (EverySet d1) (EverySet d2) =
    EverySet <| EveryDict.diff d1 d2


{-| Convert a set into a list, sorted from lowest to highest.
-}
toList : EverySet a -> List a
toList (EverySet d) =
    EveryDict.keys d


{-| Convert a list into a set, removing any duplicates.
-}
fromList : List a -> EverySet a
fromList xs =
    List.foldl insert empty xs


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : (a -> b -> b) -> b -> EverySet a -> b
foldl f b (EverySet d) =
    EveryDict.foldl (\k _ b -> f k b) b d


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : (a -> b -> b) -> b -> EverySet a -> b
foldr f b (EverySet d) =
    EveryDict.foldr (\k _ b -> f k b) b d


{-| Map a function onto a set, creating a new set with no duplicates.
-}
map : (a -> a2) -> EverySet a -> EverySet a2
map f s =
    fromList (List.map f (toList s))


{-| Create a new set consisting only of elements which satisfy a predicate.
-}
filter : (a -> Bool) -> EverySet a -> EverySet a
filter p (EverySet d) =
    EverySet <| EveryDict.filter (\k _ -> p k) d


{-| Create two new sets; the first consisting of elements which satisfy a
predicate, the second consisting of elements which do not.
-}
partition : (a -> Bool) -> EverySet a -> ( EverySet a, EverySet a )
partition p (EverySet d) =
    let
        ( p1, p2 ) =
            EveryDict.partition (\k _ -> p k) d
    in
    ( EverySet p1, EverySet p2 )
