{-| extensible effects. uses:

* the @free@ package,
for 'Free' (which transforms a functor into a monad)
and 'Cofree' (which transforms a functor into a comonad).
* the @vinyl@ package,
for (TODO) 'CoRec' (an n-ary, un-nested sum type)
and 'Rec' (an n-ary, un-nested product type).

background (necessary only for contributing to the library, not using it):

* for composing domain-specific languages and interpeters (parts 1 to 5):
    * <http://dlaing.org/cofun/posts/free_and_cofree.html>
    * <http://dlaing.org/cofun/posts/monad_transformers_and_comonad_transformers.html>
    * <http://dlaing.org/cofun/posts/coproducts_for_free_and_products_for_cofree.html>
    * <http://dlaing.org/cofun/posts/pairing_and_io.html>
    * <http://dlaing.org/cofun/posts/pairing_over_the_network.html>
    * (this series a tour-de-force, read it!).

* for "free monads": <> TODO tekmos blog
* : <> TODO
* for "extensible effects": <> TODO ocharles
* for @mtl@: <> TODO

-}
module Vinyl.Effects
  ( module Vinyl.Effects.Language
  , module Vinyl.Effects.Interpreter
  , module Vinyl.Effects.Types
  , module Data.Vinyl
  , module Vinyl.CoRec
  ) where

import Vinyl.Effects.Language
import Vinyl.Effects.Interpreter
import Vinyl.Effects.Types

import Data.Vinyl -- as Vinyl
import Vinyl.CoRec -- as Vinyl
