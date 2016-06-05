{-# LANGUAGE ConstraintKinds, DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-| This module defines two effects ('Clipboard' and 'OpenUrl'),
and then composes them ('Workflow') "on-the-fly".

For each effect, we:

* Define a functor (e.g. 'ClipboardF').
* (optionally, for convenience, define aliases for the constraint
(e.g. 'MonadClipboard') and effect-set (e.g. 'Clipboard')).
* Define overloaded constructors (e.g. 'getClipboard', 'setClipboard'). TODO th.
* Define a handler (e.g. @('handleClipboard' :: 'ClipboardF' ('IO' a) -> 'IO' a)@),
which involves minimal boilerplate. (if you've used the @free@ package,
you know how it's done).
Then, wrap that handler (e.g. with the shape @'ClipboardF' a -> a@)
in an 'Interpreter', for /extensibility/.

You can use these effects extensibly, "@mtl@-style". e.g.
Since 'getClipboard' and 'openUrl' are overloaded, they can both be used in
'openFromClipboard'.

@
'openFromClipboard' = do   -- :: ('MonadClipboard' m, 'MonadOpenUrl' m) => m ()
  s \<- 'getClipboard'      -- :: ('MonadClipboard' m                ) => m String
  'openUrl' s              -- :: (                  'MonadOpenUrl' m) => m ()
@

Note:

* the type of @openFromClipboard@ is inferred.
* the constraints are aliases; you don't need to write a new class
for each new effect type.
* for compositions of effects (like 'MonadWorkflow'),
you don't even need to write a new type.
Just append the interpreters you want (with `appendInterpreters`).

-}
module Vinyl.Effects.Example
 ( main

 -- * Effect #1: Clipboard
 ,MonadClipboard
 ,Clipboard
 ,ClipboardF(..)
 -- ** overloaded constructors
 ,getClipboard
 ,setClipboard
 -- ** e.g. reverseClipboard
 ,reverseClipboard
 -- ** the interpreter
 ,runClipboard
 ,interpretClipboard
 ,interpretClipboard2
 ,handleClipboard
 -- ** the implementation
 ,sh_GetClipboard
 ,sh_SetClipboard

 -- * Effect #2: Clipboard
 ,MonadOpenUrl
 ,OpenUrl
 ,OpenUrlF
 -- ** overloaded constructors
 ,openUrl
 -- ** the interpreter
 ,runOpenUrl
 ,interpretOpenUrl
 ,handleOpenUrl
 -- ** the implementation
 ,sh_OpenUrl

 -- * Workflow: \#1 + \#2
 , MonadWorkflow
 , Workflow
 , runWorkflow
 , interpretWorkflow1
 , interpretWorkflow2
 ,interpretOpenUrl2

 -- ** e.g. openFromClipboard
 , openFromClipboard
 , openFromClipboard_nothingSpecialized
 , openFromClipboard_monadSpecialized
 , openFromClipboard_effectsSpecialized
 , openFromClipboard_bothSpecialized

 -- * Reader, as an effect
 ,MonadReader
 ,Reader
 ,ReaderF
 ,ask

 -- * Writer, as an effect
 ,MonadWriter
 ,Writer
 ,WriterF
 ,tell

 -- * State, as an effect
 ,MonadState
 ,State
 ,StateF
 ,get
 ,put

 -- ** instance MonadLanguge RWS
 , RWS(..)
 , runRWS
 , liftRWS
 , liftReaderF
 , liftWriterF
 , liftStateF
 , exampleRWS

 ) where
import Vinyl.Effects
-- import Vinyl.Effects.Interpreter.Cofree as I

-- import Data.Vinyl (rget)
-- import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Writer (WriterT(..))
import Control.Monad.Trans.State (StateT(..))
import Data.Functor.Identity (Identity(..))

import Data.Proxy (Proxy(..))
-- import Data.Function ((&))
import Control.Arrow ((>>>))
import System.Process (CreateProcess(..), StdStream(..), createProcess, waitForProcess, proc, shell)
import GHC.IO.Handle (hGetContents)

--------------------------------------------------------------------------------

{-| run with:

@
stack build && stack exec example-vinyl-effects
@

(read the source too).

-}
main :: IO ()
main = do
 putStrLn ""

 -- runOpenUrl $ openUrl "http://google.com"

 -- runClipboard $ setClipboard "'" -- { echo '''' | pbcopy } would fail, unless propertly escaped

 -- runClipboard $ setClipboard "http://google.com"
 -- runWorkflow $ openFromClipboard
 --
 -- runClipboard $ reverseClipboard
 -- print =<< runClipboard getClipboard

 runClipboard $ do
   setClipboard "http://google.com"   -- `setClipboard` as a `Clipboard`

 contents <- runWorkflow $ do
  --  openFromClipboard
   reverseClipboard                   -- `setClipboard` as a `Workflow`
   getClipboard

 print contents

 let ((a::Int), (w::[String]), (s::Int)) = runRWS False 1 $ do
       -- _ <- exampleRWS --Err No instance for (Num t0) arising from a use of ‘exampleRWS’
       (_::Int) <- exampleRWS --TODO Can it be inferred? Constraint trick?
       exampleRWS
 print (a,w,s)


--------------------------------------------------------------------------------

{- | an effect to visit the url that's currently in the clipboard.

uses two distinct effects, i.e. it's a 'Workflow' action.

@
openFromClipboard = do
  s <- 'getClipboard'
  'openUrl' s
@

Inferred (with @NoMonomorphismRestriction@):

@
 :: ( 'MonadClipboard' m effects
    , 'MonadOpenUrl' m effects
    )
 => m ()
@

(the same, without aliases)

@
 :: ( 'MonadLanguage' m effects
    , 'RElem' ClipboardF effects ('RIndex' ClipboardF effects)
    , 'RElem' OpenUrlF   effects ('RIndex' OpenUrlF   effects)
    )
 => m ()
@

(which is what haddock displays, unformatted).

i.e. "any monad, that supports any set of effects that have at least 'ClipboardF' and 'OpenUrlF'".

you can specialize the effects:

@
openFromClipboard
 :: ('MonadLanguage' m [ClipboardF, OpenUrlF])
 => m ()
@

i.e. "any monad, that supports exactly two effects, 'ClipboardF' and 'OpenUrlF'".

or the monad:

@
openFromClipboard
  :: (ClipboardF ∈ effects, OpenUrlF ∈ effects)
  => 'Language' effects ()
@

or both:

@
openFromClipboard
 () =>
 :: 'Language' [ClipboardF, OpenUrlF] ()
@

-}
openFromClipboard = do
  s <- getClipboard
  openUrl s

-- | @= 'openFromClipboard'@
openFromClipboard_nothingSpecialized :: (MonadWorkflow m effects) => m ()
openFromClipboard_nothingSpecialized = openFromClipboard

-- | @= 'openFromClipboard'@
openFromClipboard_effectsSpecialized
 :: (MonadLanguage m [ClipboardF, OpenUrlF])
 => m ()
openFromClipboard_effectsSpecialized = openFromClipboard

-- | @= 'openFromClipboard'@
openFromClipboard_monadSpecialized
  :: (ClipboardF ∈ effects, OpenUrlF ∈ effects)
  => Language effects ()
openFromClipboard_monadSpecialized = openFromClipboard
--old   :: ([ClipboardF, OpenUrlF] ⊆ effects)

-- | @= 'openFromClipboard'@
openFromClipboard_bothSpecialized
 :: ()
 => Language [ClipboardF, OpenUrlF] ()
openFromClipboard_bothSpecialized = openFromClipboard


--------------------------------------------------------------------------------

-- | a constraint (with @-XConstraintKinds@).
type MonadWorkflow m effects =
   ( MonadClipboard m effects
   , MonadOpenUrl   m effects
   )

-- | a set of two effects.
type Workflow = '[ClipboardF,OpenUrlF]

{-| run an ad-hoc grouping of two effects.

@
runWorkflow = 'interpretLanguage' interpretWorkflow1
@

can run any action of type:

@('MonadWorkflow' m effects) => m a@

-}
runWorkflow :: Language Workflow :~> IO
runWorkflow = interpretLanguage interpretWorkflow1

{-old
runWorkflow :: Language Workflow a -> IO a
-}

{- | definition #1:  compose interpreters by appending vinyl records.

@
interpretWorkflow = 'appendInterpreters' 'interpretClipboard' 'interpretOpenUrl'
@

no new @Either@-like @data@types needed,
the @type@-aliases are only for clarity.

-}
interpretWorkflow1 :: Interpreter IO Workflow
interpretWorkflow1 = interpretClipboard `appendInterpreters` interpretOpenUrl

{- | definition #2: Construct an interpreter directly, via handlers.

@
'Interpreter'
  $ 'HandlerM' 'handleClipboard'
 :& 'HandlerM' 'handleOpenUrl'
 :& RNil
@

-}
interpretWorkflow2 :: Interpreter IO Workflow
interpretWorkflow2 = Interpreter
  $ HandlerM handleClipboard
 :& HandlerM handleOpenUrl
 :& RNil

{-| If we can handle an effect, plus some others;
then we can handle that effect, alone.

@
'interpretOpenUrl2' = 'downcastInterpreter' 'interpretWorkflow1'
@

This casts @\'['ClipboardF','OpenUrlF']@ down to @\'['OpenUrlF']@.

(For example, some library exports only a single interpreter
that handles five effects.
We can reconstruct an interpreter that handles only three
of those effects with a one-liner).

-}
interpretOpenUrl2 :: Interpreter IO OpenUrl
interpretOpenUrl2 = downcastInterpreter interpretWorkflow1

--------------------------------------------------------------------------------

-- | the constraint
type MonadClipboard m effects =
  ( MonadLanguage m effects
  , ClipboardF ∈ effects
  )

-- | the set of effects (one)
type Clipboard = '[ClipboardF]

-- | the functor
data ClipboardF k
 = GetClipboard (String -> k)
 | SetClipboard String k
 deriving Functor

-- | @getClipboard = 'liftL' $ 'GetClipboard' id@
getClipboard :: (MonadClipboard m effects) => m String
getClipboard = liftL $ GetClipboard id
   -- GetClipboard id :: ClipboardF String

-- | @setClipboard s = 'liftL' $ 'SetClipboard' s ()@
setClipboard :: (MonadClipboard m effects) => String -> m ()
setClipboard s = liftL $ SetClipboard s ()
  -- SetClipboard s () :: ClipboardF ()

-- | derived from the two primitves.
reverseClipboard :: (MonadClipboard m effects) => m ()
reverseClipboard = getClipboard >>= (reverse >>> setClipboard)

{- | calls 'interpretLanguage'.

when using free monads directly, you would:

@
runClipboard = 'iterTM' handleClipboard
@

-}
runClipboard :: Language '[ClipboardF] :~> IO
runClipboard = interpretLanguage interpretClipboard

{- | definition #1:
"inject" a  handler into an interpreter with 'singletonInterpreter'.

@
'singletonInterpreter' 'handleClipboard'
@

-}
interpretClipboard :: Interpreter IO '[ClipboardF]
interpretClipboard = singletonInterpreter handleClipboard

{- | definition #2:
 constructed and interpreted directly from single handler.

@
= 'Interpreter'
  $ 'HandlerM' 'handleClipboard'
  ':&' 'RNil'
@

-}
interpretClipboard2 :: Interpreter IO '[ClipboardF]
interpretClipboard2 = Interpreter
   $ HandlerM handleClipboard
  :& RNil

{- | glue the functor to its effects.

@
handleClipboard = \\case
  'GetClipboard' f   -> 'sh_GetClipboard' '>>=' f
  'SetClipboard' s k -> 'sh_SetClipboard' s '>>' k
@

-}
handleClipboard :: AnAlgebra ClipboardF (IO a)
handleClipboard = \case
  GetClipboard f -> sh_GetClipboard >>= f
  SetClipboard s k -> sh_SetClipboard s >> k

-- | shells out (@$ pbpaste@), works only on OSX.
sh_GetClipboard :: IO String
sh_GetClipboard = do
  -- TODO readProcess
  (_in, Just _out, _err, _process) <- createProcess  --NOTE safe
      (proc "pbpaste" [])
      { std_out = CreatePipe }
  out <- hGetContents _out
  let s = init out -- strip trailing \n  --TODO NOTE safe
  return s

-- | shells out (@$ ... | pbcopy@), works only on OSX. blocking.
sh_SetClipboard :: String -> IO ()
sh_SetClipboard s = do
  (_in, _out, _err, _process) <- createProcess $
      (shell $ "echo '"++s++"' | pbcopy") -- lol. TODO escape?
  _ <- waitForProcess _process
  return ()

--------------------------------------------------------------------------------
--
-- -- | the set of handlers (one)
-- type CoClipboard = '[CoClipboardF]
--
-- {- | the dual functor:
--
-- * the sum (@ data ... = ... | ...@) becomes
-- a product (@ data ... = ... { ..., ... }@)
-- * @(->)@ becomes @(,)@ and vice versa
--
-- because as 'ClipboardF' "produces" values,
-- so 'CoClipboardF' "consumes" them.
--
-- -}
-- data CoClipboardF k = CoClipboardF
--  { _getClipboard :: (String, k)  -- TODO {String} should be {m String}; unlike _setClipboard, {k ~ m ()} wont work
--  , _setClipboard :: String -> k
--  }
--  deriving Functor
--
-- -- pairClipboardT :: Pairing CoClipboardT ClipboardT
-- -- pairClipboardT = pairClipboardF
--
-- pairClipboardF :: Pairing CoClipboardF ClipboardF
-- pairClipboardF = Pairing go
--  where
--  go :: (a -> b -> r) -> (CoClipboardF a -> ClipboardF b -> r)
--  go p CoClipboardF{..} = \case
--    GetClipboard   f -> let (s,a) = _getClipboard
--                            b = f s
--                        in  p a b
--    SetClipboard s b -> let a = _setClipboard s
--                        in  p a b

--------------------------------------------------------------------------------

-- | the constraint
type MonadOpenUrl m effects =
  ( MonadLanguage m effects
  , OpenUrlF ∈ effects
  )

-- | the set of effects (one)
type OpenUrl = '[OpenUrlF]

-- | the functor
data OpenUrlF k -- TODO name OpenFile, works for any file
 = OpenUrl String k
 deriving Functor

-- | @openUrl s = 'liftL' $ 'OpenUrl' s ()@
openUrl :: (MonadOpenUrl m effects) => String -> m ()
openUrl s = liftL $ OpenUrl s ()
   -- OpenUrl s :: OpenUrlF ()

{- |

@
runOpenUrl = interpretLanguage interpretOpenUrl
@

-}
runOpenUrl :: Language '[OpenUrlF] :~> IO
runOpenUrl = interpretLanguage interpretOpenUrl

{- |

@
interpretOpenUrl = 'singletonInterpreter' $ \case
  'OpenUrl' s k -> 'sh_OpenUrl' s >> k
@

can extract the "co-algebra" with

@
handleOpenUrl = 'fromSingletonInterpreter' interpretOpenUrl
@

-}
interpretOpenUrl :: Interpreter IO '[OpenUrlF]
interpretOpenUrl = singletonInterpreter handleOpenUrl

{- | glue the functor to its effects.

@
handleOpenUrl = \\case
  'OpenUrl' s k -> 'sh_OpenUrl' s '>>' k
@

-}
handleOpenUrl :: AnAlgebra OpenUrlF (IO a)
handleOpenUrl = \case
  OpenUrl s k -> sh_OpenUrl s >> k

-- | shells out (@$ open ...@), should work cross-platform. blocking.
sh_OpenUrl :: String -> IO ()
sh_OpenUrl s = do
  (_in, _out, _err, _process) <- createProcess
      (proc "open" [s])
  _ <- waitForProcess _process
  return ()

--------------------------------------------------------------------------------

-- | the constraint
type MonadReader r m effects =
  ( MonadLanguage m effects
  , ReaderF r ∈ effects
  )

-- | the set of effects (one)
type Reader r = '[ReaderF r]

-- | the functor
data ReaderF r k
 = Ask (r -> k)
 deriving Functor

-- | @ask = 'liftL' $ 'Ask' id@
ask :: (MonadReader r m effects) => m r
ask = liftL $ Ask id

--------------------------------------------------------------------------------

-- | the constraint
type MonadWriter w m effects =
  ( MonadLanguage m effects
  , WriterF w ∈ effects
  )

-- | the set of effects (one)
type Writer w = '[WriterF w]

-- | the functor
data WriterF w k
 = Tell w k
 deriving Functor

-- | @tell w = 'liftL' $ 'Tell' w ()@
tell :: (MonadWriter w m effects) => w -> m ()
tell w = liftL $ Tell w ()

--------------------------------------------------------------------------------

-- | the constraint
type MonadState s m effects =
  ( MonadLanguage m effects
  , StateF s ∈ effects
  )

-- | the set of effects (one)
type State s = '[StateF s]

-- | the functor
data StateF s k
 = Get (s -> k)
 | Put s k
 deriving Functor

-- | @get = 'liftL' $ 'Get' id@
get :: (MonadState s m effects) => m s
get = liftL $ Get id

-- | @put s = 'liftL' $ 'Put' s ()@
put :: (MonadState s m effects) => s -> m ()
put s = liftL $ Put s ()

--------------------------------------------------------------------------------

{-|

-}
newtype RWS r w s a = RWS { getRWS ::
 ReaderT r (WriterT w (StateT s Identity)) a
 }
 deriving (Functor,Applicative,Monad)

-- |
runRWS :: (Monoid w) => r -> s -> RWS r w s a -> (a,w,s)
runRWS r s
    = getRWS
  >>> flip runReaderT r
  >>> runWriterT
  >>> flip runStateT s
  >>> runIdentity
  >>> (\((_a,_w),_s) -> (_a,_w,_s))

{-| since 'MonadLanguage is a class,
even though 'RWS' is a custom monad (not a 'Language'),
you can still provide an instance.


-}
instance (Monoid w) => MonadLanguage (RWS r w s) [ReaderF r, WriterF w, StateF s] where
  -- liftL :: (f ∈ effects, Functor f) => f a -> m a
  liftL effect = getOpNaturalTransformation (rget Proxy liftRWS) effect

{-old
liftL effect = getOpNaturalTransformation (rget effect liftRWS) effect
-}

{- |

-}
liftRWS
 :: (Monoid w)
 => Rec (OpNaturalTransformation (RWS r w s)) [ReaderF r, WriterF w, StateF s]
liftRWS
   = OpNaturalTransformation liftReaderF
  :& OpNaturalTransformation liftWriterF
  :& OpNaturalTransformation liftStateF
  :& RNil

-- |
liftReaderF :: (Monoid w) => ReaderF r a -> RWS r w s a
liftReaderF = \case
 Ask f -> RWS $ ReaderT $ \r -> do
   return (f r)
 -- ReaderT r m a = r -> m a

-- |
liftWriterF :: (Monoid w) => WriterF w a -> RWS r w s a
liftWriterF = \case
 Tell w k -> RWS $ ReaderT $ \_ -> WriterT $ do
   return (k,w)
 -- WriterT w m a = m (a, w)

-- |
liftStateF :: (Monoid w) => StateF s a -> RWS r w s a
liftStateF = \case
 Get f   -> RWS $ ReaderT $ \_ -> WriterT $ StateT $ \s -> do
   return ((f s, mempty), s) --TODO?
 Put s k -> RWS $ ReaderT $ \_ -> WriterT $ StateT $ \_ -> do
   return ((k,mempty), s)
 -- StateT s m a = s -> m (a,s)

{-NOTE

ReaderT r (WriterT w (StateT s Identity)) a
~
r -> WriterT w (StateT s Identity) a
~
r -> StateT s Identity (a,w)
~
r -> Identity ((a,w),s)
~
r -> ((a,w),s)

-}

--------------------------------------------------------------------------------

-- |
exampleRWS
 :: ( MonadReader Bool     m effects
    , MonadWriter [String] m effects
    , MonadState  i        m effects
    , Num i, Show i
    )
 => m i
exampleRWS = do
  b <- ask
  i <- get

  let f = if b then id else negate
  let j = f i
  let k = i + 1

  tell $ [show j]
  put k
  return j

--------------------------------------------------------------------------------
