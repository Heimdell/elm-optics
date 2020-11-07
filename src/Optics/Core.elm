module Optics.Core exposing
    ( Y, Optic, Lens, Prism, Traversal, Iso
    , SimpleLens, SimplePrism, SimpleTraversal, SimpleIso
    , lens, prism, traversal, iso
    , id, o
    , get
    , review, is
    , getSome, getAll, update, assign
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

@docs get


# `Prism` usage

@docs review, is


# General usage (`Traversal`s)

@docs getSome, getAll, update, assign

-}

import Either exposing (Either(..))


{-| Use this type as replacement for `pr`/`ls` variable when they are in the
signature of the function that calls any of requiring eliminators (`get`/
`review`/`is`).
-}
type alias Y =
    ()


{-| Type-level "no". Is a 0 type.
-}
type N
    = N N


{-| A lens, prism or a traversal.

The `pr` and `ls` are used to track `review` and `get` capabilities, leave them being type variables. The consumers of lens will try to unify either of these with `Y` type (= `()`).

The `s`, `t`, `a` and `b` are "input object type", "output object type", "input part type"
and "output part type", accordingly.

This means that you can get `a` from `s`, and given a function `a -> b` make a `t` out of `s`.

Later in this text I will use term "final" for "out" types and "initial" for "in" types.

-}
type Optic pr ls s t a b
    = Optic
        { get : ( ls, s ) -> a
        , review : ( pr, b ) -> t
        , getAll : s -> List a
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


{-| The isomorphism is both "lens" and "prism".
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
        { get = \( _, a ) -> v a
        , getAll = \a -> [ v a ]
        , review = \( n, b ) -> absurd n
        , update = \f s -> upd s <| f <| v s
        }


{-| A prism constructor.

Parameters are: reconstructor and a splitter.

Reconstructor takes a final value and constructs a final object.

The splitter turns initial object either to final object directly (if initial object is of wrong variant),
or spits out `a`.

-}
prism : (b -> t) -> (s -> Either t a) -> Prism pr s t a b
prism back split =
    Optic
        { getAll = split >> Either.unpack (always []) (\a -> [ a ])
        , get = \( n, s ) -> absurd n
        , review = \( _, b ) -> back b
        , update =
            \f -> split >> Either.unpack identity (f >> back)
        }


{-| A traversal constructor.

Parameters are: toList and a mapper.

We need `toList`, because there is no `Foldable` typeclass in Elm.

The mapper is a "mapSomething" function update `s`.

-}
traversal : (s -> List a) -> ((a -> b) -> s -> t) -> Traversal s t a b
traversal v u =
    Optic
        { getAll = v
        , get = \( n, _ ) -> absurd n
        , review = \( n, _ ) -> absurd n
        , update = u
        }


{-| An isomorphism constructor.
-}
iso : (s -> a) -> (b -> t) -> Iso pr ls s t a b
iso v upd =
    Optic
        { get = \( _, a ) -> v a
        , getAll = \a -> [ v a ]
        , review = \( _, b ) -> upd b
        , update = \f s -> upd <| f <| v s
        }


{-| An identity optic.
-}
id : SimpleLens ls s s
id =
    Optic
        { getAll = List.singleton
        , get = Tuple.second
        , review = Tuple.second
        , update = identity
        }


{-| Optical composition.
-}
o : Optic pr ls s t a b -> Optic pr ls a b x y -> Optic pr ls s t x y
o (Optic f) (Optic g) =
    Optic
        { getAll = f.getAll >> List.concatMap g.getAll
        , get = \( y, s ) -> g.get ( y, f.get ( y, s ) )
        , review = \( y, b ) -> f.review ( y, g.review ( y, b ) )
        , update = g.update >> f.update
        }


{-| Retrieve the only element using a lens.
-}
get : Optic pr Y s t a b -> s -> a
get (Optic l) s =
    l.get ( (), s )


{-| Retrieve up to one element using any optic.
-}
getSome : Optic pr ls s t a b -> s -> Maybe a
getSome (Optic l) =
    l.getAll >> List.head


{-| Check if s is related to a prism.

**Note:** You can change `Y` to `pr` here and it will still compile.
I restricted it to prisms, because semantics for traversals
is kinda be questionable.

-}
is : Optic Y ls s t a b -> s -> Bool
is (Optic l) =
    l.getAll >> List.isEmpty >> not


{-| Retrieve all elements using any optic.
-}
getAll : Optic pr ls s t a b -> s -> List a
getAll (Optic l) =
    l.getAll


{-| Use prism to reconstruct.
-}
review : Optic Y ls s t a b -> b -> t
review (Optic l) s =
    l.review ( (), s )


{-| Update over any optic.
-}
update : Optic pr ls s t a b -> (a -> b) -> (s -> t)
update (Optic l) =
    l.update


{-| Assign into any optic.
-}
assign : Optic pr ls s t a b -> b -> (s -> t)
assign (Optic l) =
    l.update << always
