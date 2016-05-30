{-# LANGUAGE ConstraintKinds, NoMonomorphismRestriction, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-| (see source).

-}
module Vinyl.Effects.Example where
import Vinyl.Effects

import Control.Arrow ((>>>))

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
sh_GetClipboard = return "the clipboard contents"

-- | shells out (@$ ... | pbcopy@), works only on OSX.
sh_SetClipboard :: String -> IO ()
sh_SetClipboard s = print s

--------------------------------------------------------------------------------

type MonadOpenUrl m effects =
  ( MonadLanguage m effects
  , OpenUrlF ∈ effects
  )

data OpenUrlF k
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

-- | shells out (@$ open ...@), works only on OSX.
sh_OpenUrl :: String -> IO ()
sh_OpenUrl s = print s

--------------------------------------------------------------------------------

{-|

@
stack build && stack exec example-vinyl-effects
@

-}
main :: IO ()
main = do
 putStrLn ""
 runMonadOpenUrl $ openUrl "google.com"
 runMonadClipboard $ reverseClipboard

--------------------------------------------------------------------------------
