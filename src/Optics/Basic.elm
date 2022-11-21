module Optics.Basic exposing
    ( just_, nothing_
    , right_, left_
    , only
    , each, nth, drop, take, cons, nil, head, tail
    , every, ith
    , first, second
    , atKey, assocs, dictValues
    )

{-| Some optics for basic types.


# Maybe

@docs just_, nothing_


# Either

@docs right_, left_


# Filter

@docs only


# List

@docs each, nth, drop, take, cons, nil, head, tail


# Array

@docs every, ith


# Tuple

@docs first, second


# Dict

@docs atKey, assocs, dictValues

-}

import Array exposing (Array(..))
import Dict exposing (Dict)
import Either exposing (Either(..))
import Html.Attributes exposing (list)
import Maybe exposing (Maybe)
import Optics.Core exposing (..)


{-| Match (go into) `Just` constructor.
-}
just_ : Prism pr (Maybe a) (Maybe b) a b
just_ =
    prism Just <|
        \s ->
            case s of
                Just a ->
                    Right a

                Nothing ->
                    Left Nothing


{-| Match `Nothing` constructor.
-}
nothing_ : SimplePrism pr (Maybe a) ()
nothing_ =
    prism (always Nothing) <|
        \s ->
            case s of
                Nothing ->
                    Right ()

                _ ->
                    Left s


{-| Match `Left` constructor.
-}
left_ : Prism pr (Either a c) (Either b c) a b
left_ =
    prism Left <| Either.unpack Right (Right >> Left)


{-| Match `Right` constructor.
-}
right_ : Prism pr (Either c a) (Either c b) a b
right_ =
    prism Right <| Either.unpack (Left >> Left) Right


{-| Pluggable filter.

Prevents reading and update for all parts that do to satisfy the predicate.

-}
only : (a -> Bool) -> SimpleTraversal a a
only pred =
    prism identity <|
        \s ->
            if pred s then
                Right s

            else
                Left s


{-| List fold/traversal.
-}
each : Traversal (List a) (List b) a b
each =
    traversal identity List.map


{-| Array fold/traversal.
-}
every : Traversal (Array a) (Array b) a b
every =
    traversal Array.toList Array.map


{-| First tuple element.
-}
first : Lens n ( a, c ) ( b, c ) a b
first =
    lens Tuple.first (\( _, b ) a -> ( a, b ))


{-| Second tuple element.
-}
second : Lens n ( c, a ) ( c, b ) a b
second =
    lens Tuple.second (\( a, _ ) b -> ( a, b ))


{-| List head.
-}
head : SimpleTraversal (List a) a
head =
    o cons first


{-| List tail.
-}
tail : SimpleTraversal (List a) (List a)
tail =
    o cons second


{-| First n elements of the list.

The implementation will traverse n elements twice, because I was lazy and didn't write List.splitAt manually.

And there is no standard one.

-}
take : Int -> SimpleTraversal (List a) (List a)
take i =
    traversal (List.drop i >> (\x -> [ x ])) <|
        \f lst ->
            f (List.take i lst) ++ List.drop i lst


{-| Everything except first n elements of the list.
-}
drop : Int -> SimpleTraversal (List a) (List a)
drop i =
    traversal (List.drop i >> (\x -> [ x ])) <|
        \f lst ->
            List.take i lst ++ f (List.drop i lst)


{-| Nth element of the list.
-}
nth : Int -> SimpleTraversal (List a) a
nth i =
    o (drop i) head


{-| Nth element of the array.
-}
ith : Int -> SimpleTraversal (Array a) a
ith i =
    traversal (Array.get i >> getAll just_) <|
        \f arr ->
            case Array.get i arr of
                Just x ->
                    Array.set i x arr

                Nothing ->
                    arr


{-| Match head/tail of a non-empty list.
-}
cons : Prism pr (List a) (List b) ( a, List a ) ( b, List b )
cons =
    prism (uncurry (::)) <|
        \lst ->
            case lst of
                x :: xs ->
                    Right ( x, xs )

                [] ->
                    Left []


{-| Match end of a non-empty list.
-}
nil : Prism pr (List a) (List a) () ()
nil =
    prism (always []) <|
        \lst ->
            case lst of
                x :: xs ->
                    Left lst

                [] ->
                    Right ()


{-| Why, Mr. Czaplicki? Why?
-}
uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


{-| Access to the dict element at given key.
-}
atKey : comparable_k -> SimpleTraversal (Dict comparable_k v) v
atKey k =
    traversal (Dict.get k >> getAll just_) <|
        \f arr ->
            case Dict.get k arr of
                Just x ->
                    Dict.insert k (f x) arr

                Nothing ->
                    arr


{-| Dict <-> List.
-}
assocs : Iso pr ls (Dict comparable_k v) (Dict comparable_l w) (List ( comparable_k, v )) (List ( comparable_l, w ))
assocs =
    iso Dict.toList Dict.fromList


{-| Traversal over values in the dictionary.
-}
dictValues : Traversal (Dict comparable_k v) (Dict comparable_k w) v w
dictValues =
    o assocs (o each second)
