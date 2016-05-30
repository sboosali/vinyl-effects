{-# LANGUAGE ConstraintKinds, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-| (see source).

-}
module Vinyl.Effects.Example where
import Vinyl.Effects

import Data.Vinyl

--------------------------------------------------------------------------------

main :: IO ()
main = do
 print ""

--------------------------------------------------------------------------------

type MonadClipboard m effects
 = (MonadLanguage m effects, ClipboardF ∈ effects)

data ClipboardF k
 = GetClipboard (String -> k)
 | SetClipboard String k
 deriving Functor

getClipboard :: (MonadClipboard m effects) => m String
getClipboard = liftL $ GetClipboard id
   -- GetClipboard id :: ClipboardF String

setClipboard :: (MonadClipboard m effects) => String -> m ()
setClipboard s = liftL $ SetClipboard s ()
  -- SetClipboard s () :: ClipboardF ()

--------------------------------------------------------------------------------

type MonadOpenUrl m effects
 = (MonadLanguage m effects, OpenUrlF ∈ effects)

data OpenUrlF k
 = OpenUrl String k
 deriving Functor

openUrl :: (MonadOpenUrl m effects) => String -> m ()
openUrl s = liftL $ OpenUrl s ()
   -- OpenUrl s :: OpenUrlF ()

--------------------------------------------------------------------------------

-- | Inferred (with @NoMonomorphismRestriction@).
-- any set of effects that have at least 'ClipboardF' and 'OpenUrlF'.
openUrlFromClipboard = do
  s <- getClipboard
  openUrl s
