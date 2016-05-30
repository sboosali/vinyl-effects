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
openUrlFromClipboard = do
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
openUrlFromClipboard
 :: ('MonadLanguage' m ['ClipboardF', 'OpenUrlF'])
 => m ()
@

i.e. "any monad, that supports exactly two effects, 'ClipboardF' and 'OpenUrlF'".

-}
openUrlFromClipboard = do
  s <- getClipboard
  openUrl s

-- | @= 'openUrlFromClipboard'@
openUrlFromClipboard_effectsSpecialized
 :: (MonadLanguage m [ClipboardF, OpenUrlF])
 => m ()

openUrlFromClipboard_effectsSpecialized = openUrlFromClipboard

--------------------------------------------------------------------------------

type MonadWorkflow m effects =
   ( MonadClipboard m effects
   , MonadOpenUrl m effects
   )

--------------------------------------------------------------------------------

type MonadClipboard m effects =
  ( MonadLanguage m effects
  , ClipboardF ∈ effects
  )

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

reverseClipboard :: (MonadClipboard m effects) => m ()
reverseClipboard = getClipboard >>= (reverse >>> setClipboard)

-- | calls 'iterLM'.
runMonadClipboard :: Language '[ClipboardF] a -> IO a
runMonadClipboard = iterLM go
 where
 go :: LanguageF '[ClipboardF] (IO a) -> IO a
 go = fromUnitLanguageF >>> \case
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

data OpenUrlF k -- TODO name OpenFile, works for any file
 = OpenUrl String k
 deriving Functor

-- | @openUrl s = 'liftL' $ 'OpenUrl' s ()@
openUrl :: (MonadOpenUrl m effects) => String -> m ()
openUrl s = liftL $ OpenUrl s ()
   -- OpenUrl s :: OpenUrlF ()

-- | calls 'iterLM'.
runMonadOpenUrl :: Language '[OpenUrlF] a -> IO a
runMonadOpenUrl = iterLM go
 where
 go :: LanguageF '[OpenUrlF] (IO a) -> IO a
 go = fromUnitLanguageF >>> \case
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
 -- runMonadOpenUrl $ openUrl "http://google.com"
 -- runMonadClipboard $ setClipboard "'" -- { echo '''' | pbcopy } would fail, unless propertly escaped

 runMonadClipboard $ reverseClipboard
 print =<< runMonadClipboard getClipboard

--------------------------------------------------------------------------------
