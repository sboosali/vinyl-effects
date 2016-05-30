{-# LANGUAGE ConstraintKinds, FlexibleContexts, KindSignatures, DataKinds, GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- {-# OPTIONS_GHC -ddump-splices #-}
-- for `makeFree`

{-|

-}
module Vinyl.Effects where
import Vinyl.Effects.Extra
import Vinyl.CoRec

import Data.Vinyl
import Control.Monad.Trans.Free
-- import Control.Monad.Free.TH       (makeFree)
-- import Control.Comonad.Trans.Cofree

import Control.Monad

--------------------------------------------------------------------------------

{-| a domain-specific @language@ that supports the @effects@.

-}
newtype Language effects a = Language { getLanguage ::
 FreeF (LanguageF effects)
       a
       (Language effects a)

 } -- deriving (Functor)

-- | 'fmap'
instance Functor (Language effects) where
 fmap f (Language m) = Language $ case m of
   Pure a  -> Pure $ f a
   Free fm -> Free $ fmap f `fmap` fm

-- | 'pure' calls 'Pure'
instance Applicative (Language effects) where
  pure = Pure >>> Language
  (<*>) = ap

-- | '>>=' may call 'Free'
instance Monad (Language effects) where
  return = pure
  (Language m) >>= k = case m of -- TODO right order?
    Pure a  -> k a
    Free fm -> (Language . Free) $ (>>= k) `fmap` fm

--------------------------------------------------------------------------------

{-| a (type-level) @sum@ of "language features".

the `expression` may use any effect in `effects`:

@
expression :: LanguageF effects
@

generalizes 'Either':

@
Either (f a) (g a)
~
LanguageF '[f,g] a
@

-}
newtype LanguageF (effects :: [* -> *]) (a :: *) = LanguageF { getLanguageF ::
 (CoRec (Apply a) effects)
 } -- deriving (Functor)
 -- TODO GADT with All Functor, for convenience

-- |
instance Functor (LanguageF effects) where
  fmap f (LanguageF (Col (Apply fa)))
   = (LanguageF . Col . Apply) (fmap f fa)

{-| "inject" an @effect@ into a set of @effects@.

generalizes 'Left' and 'Right':

* @Left  ~ (liftE :: f a -> LanguageF '[f,g] a)@
* @Right ~ (liftE :: g a -> LanguageF '[f,g] a)@

-}
liftE
 :: ( effect ∈ effects
    , Functor effect
    )
 => effect a
 -> LanguageF effects a
liftE = Apply >>> Col >>> LanguageF
-- the type at each step (including input and output):
--
-- effect a
-- (Apply a) effect
-- CoRec (Apply a) effects
-- LanguageF effects a

--------------------------------------------------------------------------------

{-| the @m@onad supports each @effect@

a "final encoding" (TODO, is it?) for injecting functors into a sum.

Analogous to 'liftF'.

e.g.

@
data ClipboardF k = GetClipboard (String -> k) | SetClipboard String k

getClipboard :: (MonadClipboard m effects) => m String
getClipboard = liftL $ GetClipboard id
   -- GetClipboard id :: ClipboardF String

setClipboard :: (MonadClipboard m effects) => String -> m ())
setClipboard s = liftL $ SetClipboard s ()
  -- SetClipboard s () :: ClipboardF ()

type MonadClipboard m effects = (MonadLanguage m effects, ClipboardF ∈ effects)
@

the @FunctionalDependency@ (i.e. @m -> effects@) says:
"a language-monad supports one set of effects".

-}
class (Monad m) => MonadLanguage m effects | m -> effects where
 liftL
   :: ( f ∈ effects
      , Functor f
      )
   => f a  --OLD effect (m a)
   -> m a

-- | The simplest concrete implementation for the interface.
--
-- Analogous to @(m ~ State s)@ for @(MonadState m s)@.
--
-- @liftL = 'liftE' >>> 'liftF'@
--
instance MonadLanguage (Language effects) effects where
 -- :: (effect ∈ effects, Functor effect) => effect a -> Language effects a
 liftL = liftE >>> liftF
 -- the type at each step (including input and output):
 --
 -- effect a
 -- LanguageF effects a
 -- (LanguageF effects) (Language effects a)
 -- FreeF (LanguageF effects) a (Language effects a)
 -- Language effects a

{-old

 liftL = liftE >>> Free >>> Language
 -- the type at each step (including input and output):
 --
 -- effect a
 -- effect (Language effects a)
 -- (LanguageF effects) (Language effects a)
 -- FreeF (LanguageF effects) a (Language effects a)
 -- Language effects a
-}

-- |
instance MonadFree (LanguageF effects) (Language effects) where
 -- wrap :: f (m a) -> m a
 -- wrap :: (LanguageF effects) ((Language effects) a) -> (Language effects) a
 wrap = Free >>> Language


--------------------------------------------------------------------------------

{- | @Apply a@ is a type-level @('$' a)@.

@f@ must be a @Functor@.

-}
data Apply a f where
  Apply :: (Functor f) => f a -> Apply a f
-- data Apply x f = Apply (f x)

--------------------------------------------------------------------------------
