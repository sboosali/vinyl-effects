{-# LANGUAGE ConstraintKinds, KindSignatures, DataKinds #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module Vinyl.Effects.Interpreter where
import Vinyl.Effects.Types
import Vinyl.Effects.Language
-- import Vinyl.Effects.Extra

import Vinyl.CoRec
import Data.Vinyl
import Data.Vinyl.TypeLevel
-- import Control.Comonad.Trans.Cofree

-- import Control.Monad
-- import Control.Comonad

--------------------------------------------------------------------------------

{-| a product of handlers.

each field holds a co-algebra:

@
f (m a) -> m a
@

e.g.

@
:: Interpreter IO '[f,g] ()
= Interpreter $ H runF :& H runG :& RNil

runF :: f (m a) -> m a

runG :: g (m a) -> m a
@

-}
data Interpreter m effects a = Interpreter { getInterpreter ::
 Rec (OpCoAlgebra (m a)) effects -- TODO forall a.
 }

-- | a co-algebra (with the two types in reverse order).
newtype OpCoAlgebra a f = OpCoAlgebra { getOpCoAlgebra ::
 f a -> a
 }

--old type HandlerM m a f = Handler f (m a) (m a)

{-old

each field holds a natural transformation:

f (m a) ->

-}

--------------------------------------------------------------------------------

{-| A @product@ of "handlers".
Generally, a (1) lifted, (2) n-ary, (3) associative product.

e.g.

@
InterpreterF [f,g] a
@

generalizes '(,)':

@
(f a, g a)
~
LanguageF '[f,g] a
@

-}
data InterpreterF effects a = InterpreterF { getInterpreterF ::
 Rec (Apply a) effects
 } -- TODO ProductF

--------------------------------------------------------------------------------


{-| interpret a language into some monad.

-}
interpretLanguage
 :: (Monad m)
 => Interpreter m effects a
 -> Language      effects a
 -> m a
interpretLanguage interpreter = iterLM (interpretLanguage_ interpreter)

-- {-|
--
-- -}
-- interpretLanguage
--  :: ()
--  =>  Interpreter m effects a
--  -> (Language      effects a -> m a)
-- interpretLanguage (Interpreter handlers) (Language features) = m
--  where
--  m = undefined "TODO"

{-| you consume a coproduct with a product of consumers i.e. you must handle every case.

generalizes 'match'.

-}
interpretLanguage_
 :: Interpreter m effects a
 -> CoAlgebra (LanguageF effects) (m a)
interpretLanguage_ (Interpreter handlers) (LanguageF (Col variant))
 = h (getApply variant)
 where
 OpCoAlgebra h = rget variant handlers
 -- pattern matching on `Col` refines the type,
 -- which we index into the handler array,
 -- (@Apply a f@ unifies with @proxy f@),
 -- to access the correct handler.

{-old
interpretLanguage_ :: Rec (OpCoAlgebra a) fs -> CoRec (Apply a) fs -> a
interpretLanguage_ handlers (Col variant) = h (getApply variant)
 where
 OpCoAlgebra h = rget variant handlers
-}

--------------------------------------------------------------------------------

{-| make a singleton interpreter from a single handler.

e.g.

@
data ClipboardF k = GetClipboard (String -> k) | SetClipboard String k deriving Functor

singletonInterpreter :: Interpreter IO '[ClipboardF] a
singletonInterpreter $ \case
 GetClipboard f   -> ... >>= f
 SetClipboard s k -> ... s >> k
@

-}
singletonInterpreter
 :: CoAlgebra f (m a)
 -> Interpreter m '[f] a
singletonInterpreter h = Interpreter hs
 where
 hs = OpCoAlgebra h :& RNil

{- | compose two interpreters. ordered.

TODO unwrap all rec ops

-}
appendInterpreters :: Interpreter m fs a -> Interpreter m gs a -> Interpreter m (fs ++ gs) a
appendInterpreters (Interpreter fs) (Interpreter gs)
  = Interpreter $ fs <+> gs

{-| inverts 'singletonInterpreter', for convenient access.

-}
fromSingletonInterpreter :: Interpreter m '[f] a -> CoAlgebra f (m a)
fromSingletonInterpreter (Interpreter (OpCoAlgebra h :& RNil)) = h
--TODO spurious non-exhaustive

--------------------------------------------------------------------------------
