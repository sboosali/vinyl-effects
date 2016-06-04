{-# LANGUAGE GADTs, RankNTypes, ConstraintKinds #-}
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

-- | An algebra.
type AnAlgebra f a = f a -> a
-- TODO naming

newtype Algebra f a = Algebra { getAlgebra ::
 AnAlgebra f a
 }

-- | a co-algebra (with the two types in reverse order).
newtype OpAlgebra a f = OpAlgebra { getOpAlgebra ::
 AnAlgebra f a
 }

--------------------------------------------------------------------------------

-- | A co-algebra.
type ACoAlgebra f a = a -> f a
-- TODO naming

newtype CoAlgebra f a = CoAlgebra { getCoAlgebra ::
 ACoAlgebra f a
 }

-- | a co-algebra (with the two types in reverse order).
newtype OpCoAlgebra a f = OpCoAlgebra { getOpCoAlgebra ::
 ACoAlgebra f a
 }

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

{- | a @ConstraintKind@.

('IsPairing' would be the constraints, if 'Pairing' were a class).

-}
type IsPairing f g = (Functor f, Functor g)

data Pairing f g = Pairing { pair ::
 forall a b r. (a -> b -> r) -> (f a -> g b -> r) -- Rank2
 }

 --------------------------------------------------------------------------------
