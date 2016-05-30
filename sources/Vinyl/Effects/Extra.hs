module Vinyl.Effects.Extra
 ( module Vinyl.Effects.Extra
 , module X
 -- , module Data.Vinyl
 , module GHC.Generics
 , module Data.Data
 ) where

import GHC.Generics (Generic)
import Data.Data (Data)

import Control.Arrow as X ((>>>))
import Data.Function as X ((&))

nothing :: (Monad m) => m ()
nothing = return ()
