
# What

Minimalistic, but faithful port of [Lens](https://hackage.haskell.org/package/lens) to Elm.

This library provides a way to access and modify elements of data located deeply inside a structure, possibly behind sum types (`Maybe`, `Either`, etc) and/or containers (`Array`, `List`, `Dict`, `Tuple`, etc).
There also are ways to define custom accessors for your own types and compose them with a single `o` operator as you like.

# Gory details

The one new exported type it defines is `Optic pr ls s t a b`.

The `pr` and `ls` are types for proofs that given optic is a "Prism" or a "Lens".
The proof types that are used are not accessible outside the library, so if you use any of types that
require them, leave them as type variables.

For instance, the type synonym `type alias Lens ls s t a b = Optic N ls s t a b` disables "Prism" behaviour for lens by setting prism proof type as inconstructible type `N`.

The `o` operator has type `Optic pr ls s t a b -> Optic pr ls a b x z -> Optic pr ls s t z s`,
so it will, if a lens is on either side, unify `pr` variable of the second optic with `N`.

Basically, `N` will contaminate any `o` chains in a given channel, making some eliminators impossibe to use.

Same goes for prisms - `ls` is set to `N`.

The traversal is when both `ls` and `pr` are set to `N` - for instance, by creating one with `traversal` or by composing a prism with a lens in either direction (e.g. `o first _Just`).

The lens/prism-specific eliminators for the optics (`view`/`review`/`is`) are requiring that the `pr` (or `ls`) should be `()`.