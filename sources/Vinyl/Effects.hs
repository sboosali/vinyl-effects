{-# LANGUAGE ConstraintKinds, FlexibleContexts, PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- {-# OPTIONS_GHC -ddump-splices #-}
-- for `makeFree`

{-|

-}
module Vinyl.Effects where
import Vinyl.Effects.Extra()

import Data.Vinyl
import Control.Monad.Trans.Free
import Control.Monad.Free.TH       (makeFree)
-- import Control.Comonad.Trans.Cofree
