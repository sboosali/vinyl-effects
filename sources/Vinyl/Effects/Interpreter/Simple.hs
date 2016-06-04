{-# LANGUAGE ConstraintKinds, KindSignatures, DataKinds, RankNTypes #-}

{-|

Making interpreters:

* 'singletonInterpreter', from a handler

Growing interpreters:

* 'appendInterpreters'

Shrinking an interpreter:

* 'downcastInterpreter', when it handles more than you need

If you want to do more with them,
just unwrap the @newtype@ and use "Data.Vinyl".

-}
module Vinyl.Effects.Interpreter.Simple where
import Vinyl.Effects.Types
import Vinyl.Effects.Language
-- import Vinyl.Effects.Extra

import Vinyl.CoRec
import Data.Vinyl
import Data.Vinyl.TypeLevel
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
newtype Interpreter (m :: * -> *) (effects :: [* -> *]) = Interpreter { getInterpreter ::
 Rec (HandlerM m) effects
 }

--------------------------------------------------------------------------------

{-| monadically handle a functor.

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

{-| A (1) lifted, (2) n-ary, (3) associative product.

In particular, a @product@ of "handlers".

@fs@ must all be @Functor@s.

e.g.

@
ProductF [f,g] a
@

generalizes '(,)':

@
ProductF '[f,g] a
~
(f a, g a)
@

-}
data ProductF fs a = ProductF { getProductF ::
 Rec (Apply a) fs
 }

--------------------------------------------------------------------------------


{-| interpret a language into some monad.

e.g.

@
interpretLanguage anInterpreter aLanguage :: m ()
@

calls 'iterLM'.

-}
interpretLanguage
 :: (Monad m)
 => Interpreter m effects
 -> (Language effects :~> m)
interpretLanguage interpreter = iterLM (interpretLanguageF interpreter)

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

{-| you consume a coproduct with a product of consumers
i.e. you must handle every case.

like 'match'.

-}
interpretLanguageF
 :: Interpreter m effects
 -> AnAlgebra (LanguageF effects) (m a)
interpretLanguageF (Interpreter handlers) (LanguageF (Col variant))
 = h (getApply variant)
 where
 HandlerM h = rget variant handlers
 -- pattern matching on `Col` refines the type,
 -- which we index into the handler array,
 -- (@Apply a f@ unifies with @proxy f@),
 -- to access the correct handler.

{-old
interpretLanguageF :: Rec (OpCoAlgebra a) fs -> CoRec (Apply a) fs -> a
interpretLanguageF handlers (Col variant) = h (getApply variant)
 where
 OpCoAlgebra h = rget variant handlers
-}

--------------------------------------------------------------------------------

{-| make a interpreter from a single handler.

e.g.

@
data ClipboardF k = GetClipboard (String -> k) | SetClipboard String k deriving Functor

clipboardInterpreter :: Interpreter IO '[ClipboardF] a
clipboardInterpreter = singletonInterpreter $ \\case
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

-}
appendInterpreters :: Interpreter m fs -> Interpreter m gs -> Interpreter m (fs ++ gs)
appendInterpreters = asInterpreter2 (<+>) --TODO Can coerce?

{-| Discard any number of handlers from the interpreter.

-}
downcastInterpreter :: (gs ⊆ fs) => Interpreter m fs -> Interpreter m gs
downcastInterpreter = asInterpreter1 rcast  --TODO Can coerce?

--------------------------------------------------------------------------------

{-TODO unwrap all rec ops

with classes?

-}

{- | Lift a (unary) function on records to interpreters (a newtype thereof).

e.g.

@
'downcastInterpreter' = asInterpreter1 'rcast'
@

-}
asInterpreter1
 :: (Rec (HandlerM m) fs -> Rec (HandlerM m) gs)
 -> (Interpreter m fs -> Interpreter m gs)
asInterpreter1 u = \fs ->
 Interpreter $ u (getInterpreter fs)

{- | Lift an operation on records to interpreters (a newtype thereof).

e.g.

@
'appendInterpreters' = asInterpreter2 ('<+>')
@

-}
asInterpreter2
 :: (Rec (HandlerM m) fs -> Rec (HandlerM m) gs -> Rec (HandlerM m) hs)
 -> (Interpreter m fs -> Interpreter m gs -> Interpreter m hs)
asInterpreter2 op = \fs gs ->
 Interpreter $ (getInterpreter fs) `op` (getInterpreter gs)

{-old
:: (Rec (HandlerM m) fs -> Rec (HandlerM m) gs -> Rec (HandlerM m) hs) -- TODO weaker, but clearer: {forall g xs. Rec g xs})

:: (forall g as bs cs. Rec g as -> Rec g bs -> Rec g cs)
-}

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
