{-# LANGUAGE ConstraintKinds, NoMonomorphismRestriction, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-| This module defines two effects ('Clipboard' and 'OpenUrl'),
and then composes them ('Workflow') "on-the-fly".

For each effect, we:

* Define a functor (e.g. 'ClipboardF').
(optionally (for convenience), aliases for the constraint
(e.g. 'MonadClipboard') and effect-set (e.g. 'Clipboard')).
* Define overloaded constructors (e.g. 'getClipboard', 'setClipboard'). TODO th.
* Define a handler (e.g. @('handleClipboard' :: 'ClipboardF' ('IO' a) -> 'IO' a)@),
which involves minimal boilerplate. (if you've used the @free@ package,
you know how it's done).
Then, wrap that handler (a 'CoAlgebra') in an 'Interpreter',
for /extensibility/.

You can use these effects extensibly, with an "@mtl@-style". e.g.
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
 ,interpreterClipboard
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
 ,interpreterOpenUrl
 -- ** the implementation
 ,sh_OpenUrl

 -- * Workflow: \#1 + \#2
 , MonadWorkflow
 , Workflow
 , runWorkflow
 , interpretWorkflow

 -- ** e.g. openFromClipboard
 , openFromClipboard
 , openFromClipboard_nothingSpecialized
 , openFromClipboard_monadSpecialized
 , openFromClipboard_effectsSpecialized
 , openFromClipboard_bothSpecialized

 ) where
import Vinyl.Effects

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

 s <- runWorkflow $ do
   openFromClipboard
   reverseClipboard                   -- `setClipboard` as a `Workflow`
   getClipboard

 print s

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
runWorkflow = 'interpretLanguage' interpretWorkflow
@

can run any action of type:

@('MonadWorkflow' m effects) => m a@

-}
runWorkflow :: Language Workflow a -> IO a
runWorkflow = interpretLanguage interpretWorkflow

{- |

@
interpretWorkflow = 'appendInterpreters' 'interpreterClipboard' 'interpreterOpenUrl'
@

no new @Either@-like @data@types needed,
the @type@-aliases are only for clarity.

-}
interpretWorkflow :: Interpreter IO Workflow a
interpretWorkflow = interpreterClipboard `appendInterpreters` interpreterOpenUrl

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
runClipboard :: Language '[ClipboardF] a -> IO a
runClipboard = interpretLanguage interpreterClipboard

-- | @interpreterClipboard = 'singletonInterpreter' 'handleClipboard'@
interpreterClipboard :: Interpreter IO '[ClipboardF] a
interpreterClipboard = singletonInterpreter handleClipboard

{- | glue the functor to its effects.

@
handleClipboard = \case
  'GetClipboard' f   -> 'sh_GetClipboard' >>= f
  'SetClipboard' s k -> 'sh_SetClipboard' s >> k
@

-}
handleClipboard :: CoAlgebra ClipboardF (IO a)
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
runOpenUrl = interpretLanguage interpreterOpenUrl
@

-}
runOpenUrl :: Language '[OpenUrlF] a -> IO a
runOpenUrl = interpretLanguage interpreterOpenUrl

{- |

@
interpreterOpenUrl = 'singletonInterpreter' $ \case
  'OpenUrl' s k -> 'sh_OpenUrl' s >> k
@

can extract the "co-algebra" with

@
handleOpenUrl = 'fromSingletonInterpreter' interpreterOpenUrl
@

-}
interpreterOpenUrl :: Interpreter IO '[OpenUrlF] a
interpreterOpenUrl = singletonInterpreter $ \case
  OpenUrl s k -> sh_OpenUrl s >> k

-- | shells out (@$ open ...@), should work cross-platform. blocking.
sh_OpenUrl :: String -> IO ()
sh_OpenUrl s = do
  (_in, _out, _err, _process) <- createProcess
      (proc "open" [s])
  _ <- waitForProcess _process
  return ()

--------------------------------------------------------------------------------
