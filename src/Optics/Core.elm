module Optics.Core exposing
    ( Optic, Lens, Prism, Traversal, Iso
    , SimpleLens, SimplePrism, SimpleTraversal, SimpleIso
    , lens, prism, traversal, iso
    , id, o
    , view
    , review, is
    , viewSome, viewAll, over, put
    )

{-| An implementation of optics in Elm.


# Optics

@docs Optic, Lens, Prism, Traversal, Iso


# Monomorphed versions

@docs SimpleLens, SimplePrism, SimpleTraversal, SimpleIso


# Constructors

@docs lens, prism, traversal, iso


# "Category"

@docs id, o


# `Lens` usage

@docs view


# `Prism` usage

@docs review, is


# General usage (`Traversal`s)

@docs viewSome, viewAll, over, put

-}

import Either exposing (Either(..))


{-| Type-level "yes". Is a 1 type.
-}
type alias Y =
    ()


{-| Type-level "no". Is a 0 type.
-}
type N
    = N N


{-| A lens, prism or a traversal.

The 'pr' and 'ls' are type variables to track `review` and `view` capabilities (or lack thereof).

The 's', 't', 'a' and 'b' are "input object type", "output object type", "input part type"
and "output part type", accordingly.

This means that you can get 'a' from 's', and given a function 'a -> b' make a 't' out of 's'.

Later in this text I will use term "final" for "out" types and "initial" for "in" types.

-}
type Optic pr ls s t a b
    = Optic
        { view : ( ls, s ) -> a
        , review : ( pr, b ) -> t
        , viewAll : s -> List a
        , update : (a -> b) -> s -> t
        }


{-| The lens is "not a prism".
-}
type alias Lens ls s t a b =
    Optic N ls s t a b


{-| The prism is "not a lens".
-}
type alias Prism pr s t a b =
    Optic pr N s t a b


{-| The traversal is neither "lens" or "prism".
-}
type alias Traversal s t a b =
    Optic N N s t a b


{-| The traversal is both "lens" and "prism".
-}
type alias Iso pr ls s t a b =
    Optic pr ls s t a b


{-| `Optic` that cannot change type of the object.
-}
type alias SimpleOptic pr ls s a =
    Optic pr ls s s a a


{-| `Lens` that cannot change type of the object.
-}
type alias SimpleLens ls s a =
    Lens ls s s a a


{-| `Prism` that cannot change type of the object.
-}
type alias SimplePrism pr s a =
    Prism pr s s a a


{-| `Traversal` that cannot change type of the object.
-}
type alias SimpleTraversal s a =
    Traversal s s a a


{-| `Iso` that cannot change type of the object.
-}
type alias SimpleIso pr ls s a =
    Iso pr ls s s a a


{-| Ex falso.
-}
absurd : N -> a
absurd (N n) =
    absurd n


{-| A lens constructor.

Parameters are: getter and setter.

If the lens can change type of an object, the setter must return
the object of a new type.

-}
lens : (s -> a) -> (s -> b -> t) -> Lens ls s t a b
lens v upd =
    Optic
        { view = \( _, a ) -> v a
        , viewAll = \a -> [ v a ]
        , review = \( n, b ) -> absurd n
        , update = \f s -> upd s <| f <| v s
        }


{-| A prism constructor.

Parameters are: reconstructor and a splitter.

Reconstructor takes a final value and constructs a final object.

The splitter turns initial object either to final object directly (if initial object is of wrong variant),
or spits out 'a'.

-}
prism : (b -> t) -> (s -> Either t a) -> Prism pr s t a b
prism back split =
    Optic
        { viewAll = split >> Either.unpack (always []) (\a -> [ a ])
        , view = \( n, s ) -> absurd n
        , review = \( _, b ) -> back b
        , update =
            \f -> split >> Either.unpack identity (f >> back)
        }


{-| A traversal constructor.

Parameters are: toList and a mapper.

We need 'toList', because there is no 'Foldable' typeclass in Elm.

The mapper is a "mapSomething" function over 's'.

-}
traversal : (s -> List a) -> ((a -> b) -> s -> t) -> Traversal s t a b
traversal v u =
    Optic
        { viewAll = v
        , view = \( n, _ ) -> absurd n
        , review = \( n, _ ) -> absurd n
        , update = u
        }


{-| An isomorphism constructor.
-}
iso : (s -> a) -> (b -> t) -> Iso pr ls s t a b
iso v upd =
    Optic
        { view = \( _, a ) -> v a
        , viewAll = \a -> [ v a ]
        , review = \( _, b ) -> upd b
        , update = \f s -> upd <| f <| v s
        }


{-| An identity optic.
-}
id : SimpleLens ls s s
id =
    Optic
        { viewAll = List.singleton
        , view = Tuple.second
        , review = Tuple.second
        , update = identity
        }


{-| Optical composition.
-}
o : Optic pr ls s t a b -> Optic pr ls a b x y -> Optic pr ls s t x y
o (Optic f) (Optic g) =
    Optic
        { viewAll = f.viewAll >> List.concatMap g.viewAll
        , view = \( y, s ) -> g.view ( y, f.view ( y, s ) )
        , review = \( y, b ) -> f.review ( y, g.review ( y, b ) )
        , update = g.update >> f.update
        }


{-| Retrieve the only element using a lens.
-}
view : Optic pr Y s t a b -> s -> a
view (Optic l) s =
    l.view ( (), s )


{-| Retrieve up to one element using any optic.
-}
viewSome : Optic pr ls s t a b -> s -> Maybe a
viewSome (Optic l) =
    l.viewAll >> List.head


{-| Check if s is related to a prism.

**Note:** You can change `Y` to `pr` here and it will still compile.
I restricted it to prisms, because semantics for traversals
is kinda be questionable.

-}
is : Optic Y ls s t a b -> s -> Bool
is (Optic l) =
    l.viewAll >> List.isEmpty >> not


{-| Retrieve all elements using any optic.
-}
viewAll : Optic pr ls s t a b -> s -> List a
viewAll (Optic l) =
    l.viewAll


{-| Use prism to reconstruct.
-}
review : Optic Y ls s t a b -> b -> t
review (Optic l) s =
    l.review ( (), s )


{-| Update over any optic.
-}
over : Optic pr ls s t a b -> (a -> b) -> (s -> t)
over (Optic l) =
    l.update


{-| Assign into any optic.
-}
put : Optic pr ls s t a b -> b -> (s -> t)
put (Optic l) =
    l.update << always
