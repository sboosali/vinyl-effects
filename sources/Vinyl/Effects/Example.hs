{-# LANGUAGE ConstraintKinds, NoMonomorphismRestriction, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-| (see source).

-}
module Vinyl.Effects.Example where
import Vinyl.Effects

import Control.Arrow ((>>>))
import System.Process (CreateProcess(..), StdStream(..), createProcess, waitForProcess, proc, shell)
import GHC.IO.Handle (hGetContents)

--------------------------------------------------------------------------------

{- | an effect to visit the url that's currently in the clipboard.

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
    , 'RElem' OpenUrlF effects ('RIndex' OpenUrlF effects)
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

type MonadWorkflow m effects =
   ( MonadClipboard m effects
   , MonadOpenUrl   m effects
   )

type Workflow = '[ClipboardF,OpenUrlF]

{-| run an ad-hoc grouping of two effects.

-}
runWorkflow :: Language Workflow a -> IO a
runWorkflow = interpretLanguage interpretWorkflow

{- |

@
interpretWorkflow = 'appendInterpreters' 'interpreterClipboard' 'interpreterOpenUrl'
@

no new @Either@-like @data@types needed, the @type@-aliases are only for clarity.

-}
interpretWorkflow :: Interpreter IO Workflow a
interpretWorkflow = interpreterClipboard `appendInterpreters` interpreterOpenUrl

--------------------------------------------------------------------------------

type MonadClipboard m effects =
  ( MonadLanguage m effects
  , ClipboardF ∈ effects
  )

type Clipboard = '[ClipboardF]

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

-- | (derived from the two primitves).
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

-- | wraps 'handleClipboard'
interpreterClipboard :: Interpreter IO '[ClipboardF] a
interpreterClipboard = singletonInterpreter handleClipboard

{- | glue the functor to its effects.

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

type MonadOpenUrl m effects =
  ( MonadLanguage m effects
  , OpenUrlF ∈ effects
  )

type OpenUrl = '[OpenUrlF]

data OpenUrlF k -- TODO name OpenFile, works for any file
 = OpenUrl String k
 deriving Functor

-- | @openUrl s = 'liftL' $ 'OpenUrl' s ()@
openUrl :: (MonadOpenUrl m effects) => String -> m ()
openUrl s = liftL $ OpenUrl s ()
   -- OpenUrl s :: OpenUrlF ()

-- | calls 'interpretLanguage'.
runOpenUrl :: Language '[OpenUrlF] a -> IO a
runOpenUrl = interpretLanguage interpreterOpenUrl

-- |
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

{-|

@
stack build && stack exec example-vinyl-effects
@

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
