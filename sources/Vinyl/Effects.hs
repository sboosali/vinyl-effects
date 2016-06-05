{-| Extensible effects. Minimal boilerplate. Decent inference.

See "Vinyl.Effects.Example" for a detailed tutorial.

Dependencies:

* the @free@ package,
for 'Free' (which transforms a functor into a monad)
and 'Cofree' (which transforms a functor into a comonad).
* the @vinyl@ package,
for (TODO) 'CoRec' (an n-ary, un-nested sum type)
and 'Rec' (an n-ary, un-nested product type).

Concepts (necessary only for contributing to the library, not using it):

* for composing domain-specific languages and interpeters (parts 1 to 5):

     * <http://dlaing.org/cofun/posts/free_and_cofree.html>
     * <http://dlaing.org/cofun/posts/monad_transformers_and_comonad_transformers.html>
     * <http://dlaing.org/cofun/posts/coproducts_for_free_and_products_for_cofree.html>
     * <http://dlaing.org/cofun/posts/pairing_and_io.html>
     * <http://dlaing.org/cofun/posts/pairing_over_the_network.html>
     * (this series a tour-de-force, read it!).

* for "extensible records":

     * <https://hackage.haskell.org/package/vinyl-0.5.2/docs/Data-Vinyl-Tutorial-Overview.html>

* for "free monads":

     * <> TODO tekmos blog

* for "extensible effects":

     * <> TODO ocharles

* for @mtl@:

     * <> TODO

Reverse Dependencies:

* packdeps.haskellers.com/reverse/vinyl-effects
* TODO <workflow-types> uses this package to maximize the effects the
platform-independent monad provides, while still preserving the richer effects
the platform-specific monads provide (in <workflow-linux>, <workflow-osx>, and
<workflow-windows>). e.g. @OSX@ can easily access the clipboard via @bash@.

Extensions:

* The effects as a type-level set, not a type-level list (from "GHC.TypeLits");
i.e. the items are unique.
(Currently, I think the duplicates are ignored).
* "extensible interpreters" via cofree comonads.
(The original motivation for this package only needed the 'Language'
to be extensible).

Existing Alternatives:

* <extensible effects>: uses @-XOverloadedInstances@. bad inference?
* <> romansch:
* <> ocharles:
* <> mtl: doesn't compose

-}
module Vinyl.Effects
  ( module Vinyl.Effects.Language
  , module Vinyl.Effects.Interpreter
  , module Vinyl.Effects.Types
  , module Data.Vinyl
  , module Vinyl.CoRec
  -- , module Control.Monad.Free -- user shouldn't need it
  ) where

import Vinyl.Effects.Language
import Vinyl.Effects.Interpreter
import Vinyl.Effects.Types

import Data.Vinyl -- as Vinyl
import Vinyl.CoRec -- as Vinyl
-- import Control.Monad.Free -- as Free
