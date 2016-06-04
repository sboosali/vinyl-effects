{-# LANGUAGE ConstraintKinds, KindSignatures, DataKinds, RankNTypes #-}

{-|

-}
module Vinyl.Effects.Interpreter.Cofree where
import Vinyl.Effects.Types
import Vinyl.Effects.Language
-- import Vinyl.Effects.Extra

import Vinyl.CoRec
import Data.Vinyl
import Data.Vinyl.TypeLevel
import Control.Comonad.Trans.Cofree

import Control.Monad
import Control.Comonad
