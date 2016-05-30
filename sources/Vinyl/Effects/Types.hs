{-# LANGUAGE GADTs #-}
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
