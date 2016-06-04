{-# LANGUAGE GADTs, RankNTypes #-}
{-| shared types.

-}
module Vinyl.Effects.Types where

--------------------------------------------------------------------------------

{- | @Apply a@ is a type-level @('$' a)@.

@f@ must be a @Functor@.

-}
data Apply a f where
  Apply :: (Functor f) => f a -> Apply a f
-- data Apply x f = Apply (f x)

getApply :: Apply a f -> f a
getApply (Apply fa) = fa

--------------------------------------------------------------------------------

newtype Algebra f a = Algebra { getAlgebra ::
 AnAlgebra f a
 }

-- | a co-algebra (with the two types in reverse order).
newtype OpAlgebra a f = OpAlgebra { getOpAlgebra ::
 AnAlgebra f a
 }

-- | An algebra.
type AnAlgebra f a = f a -> a
-- TODO naming

--------------------------------------------------------------------------------

newtype CoAlgebra f a = CoAlgebra { getCoAlgebra ::
 ACoAlgebra f a
 }

-- | a co-algebra (with the two types in reverse order).
newtype OpCoAlgebra a f = OpCoAlgebra { getOpCoAlgebra ::
 ACoAlgebra f a
 }

-- | A co-algebra.
type ACoAlgebra f a = a -> f a
-- TODO naming

--------------------------------------------------------------------------------

-- | A natural transformation.
type (:~>) f g = forall x. f x -> g x

-- | A natural transformation.
type ANaturalTransformation f g = (:~>) f g

-- |
newtype NaturalTransformation f g = NaturalTransformation { getNaturalTransformation ::
 f :~> g
 }

-- | @~ 'NaturalTransformation' 'Identity'@
newtype IdNaturalTransformation f = IdNaturalTransformation { getIdNaturalTransformation ::
 forall x. ACoAlgebra f x
 }

-- |
newtype OpNaturalTransformation g f = OpNaturalTransformation { getOpNaturalTransformation ::
  f :~> g
 }

-- | @~ 'OpNaturalTransformation' 'Identity'@
newtype IdOpNaturalTransformation f = IdOpNaturalTransformation { getIdOpNaturalTransformation ::
 forall x. AnAlgebra f x
 }

--------------------------------------------------------------------------------
