module Vinyl.Effects.Extra
 ( module Vinyl.Effects.Extra
 -- , module Data.Vinyl
 , module GHC.Generics
 , module Data.Data
 ) where

import GHC.Generics (Generic)
import Data.Data (Data)

nothing :: Monad m => m ()
nothing = return ()
