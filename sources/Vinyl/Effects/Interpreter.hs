{-# LANGUAGE ConstraintKinds, KindSignatures, DataKinds, RankNTypes #-}
-- {-# LANGUAGE RoleAnnotations, GeneralizedNewtypeDeriving #-}
-- ExistentialQuantification

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
-- import Data.Coerce

--------------------------------------------------------------------------------

{-| a product of handlers.

each field holds a co-algebra:

@
f (m a) -> m a
@

e.g.

@
runFG :: Interpreter IO '[f,g]
runFG = Interpreter $ 'HandlerM' runF :& 'HandlerM' runG :& RNil

runF :: f (m a) -> m a

runG :: g (m a) -> m a
@

specialization:

@
Interpreter IO [f,g]
~
Rec ('HandlerM' IO) [f,g]
~
(HandlerM IO f, HandlerM IO g)
~
( (forall x. f (IO x) -> IO x)
, (forall y. g (IO y) -> IO y)
)
@

-}
data Interpreter (m :: * -> *) (effects :: [* -> *]) = Interpreter { getInterpreter ::
 Rec (HandlerM m) effects
 }

--------------------------------------------------------------------------------

{-|

-}
newtype HandlerM m f = HandlerM { getHandlerM ::
 forall x. f (m x) -> m x
 }

-- {-|
--
-- -}
-- newtype Handler f = Handler { getHandler ::
--  forall x. f x -> x
--  }
-- TODO Conflicts with co-records

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
 => Interpreter m effects
 -> Language      effects a
 -> m a
interpretLanguage interpreter = iterLM (interpretLanguage_ interpreter)

-- {-|
--
-- -}
-- interpretLanguage
--  :: ()
--  =>  Interpreter m effects
--  -> (Language      effects a -> m a)
-- interpretLanguage (Interpreter handlers) (Language features) = m
--  where
--  m = undefined "TODO"

{-| you consume a coproduct with a product of consumers i.e. you must handle every case.

generalizes 'match'.

-}
interpretLanguage_
 :: Interpreter m effects
 -> AnAlgebra (LanguageF effects) (m a)
interpretLanguage_ (Interpreter handlers) (LanguageF (Col variant))
 = h (getApply variant)
 where
 HandlerM h = rget variant handlers
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
 :: (forall x. AnAlgebra f (m x))
 -> Interpreter m '[f]
singletonInterpreter h = Interpreter hs
 where
 hs = HandlerM h :& RNil

{- | compose two interpreters. ordered.

TODO unwrap all rec ops

-}
appendInterpreters :: Interpreter m fs -> Interpreter m gs -> Interpreter m (fs ++ gs)
appendInterpreters (Interpreter fs) (Interpreter gs)
  = Interpreter $ fs <+> gs

{-err
appendInterpreters = coerce (<+>)

Couldn't match representation of type ‘Rec f0 (as0 ++ bs0)’
                         with that of ‘Interpreter m (fs ++ gs) a’
arising from trying to show that the representations of
  ‘Rec f0 as0 -> Rec f0 bs0 -> Rec f0 (as0 ++ bs0)’ and
  ‘Interpreter m fs a
   -> Interpreter m gs a -> Interpreter m (fs ++ gs) a’ are the same
Relevant role signatures:
 type role Rec nominal representational nominal
 type role ++ nominal nominal nominal
 type role Interpreter nominal nominal nominal
-}

{-| inverts 'singletonInterpreter', for convenient access.

-}
fromSingletonInterpreter :: Interpreter m '[f] -> AnAlgebra f (m a)
fromSingletonInterpreter (Interpreter (HandlerM h :& RNil)) = h
--TODO spurious non-exhaustive

--------------------------------------------------------------------------------
