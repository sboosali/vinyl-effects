{-# LANGUAGE ConstraintKinds, NoMonomorphismRestriction, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-| (see source).

-}
module Vinyl.Effects.Example where
import Vinyl.Effects

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

--------------------------------------------------------------------------------

-- |
main :: IO ()
main = do
 print "MonadWorkflow"

--------------------------------------------------------------------------------
