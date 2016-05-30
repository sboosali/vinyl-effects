{-# LANGUAGE ConstraintKinds, KindSignatures, DataKinds #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module Vinyl.Effects.Interpreter where
import Vinyl.Effects.Types
-- import Vinyl.Effects.Extra
-- import Vinyl.CoRec

import Data.Vinyl
-- import Control.Comonad.Trans.Cofree

-- import Control.Monad
-- import Control.Comonad

--------------------------------------------------------------------------------

{-| a @product@ of "handlers", one per "language feature".

e.g.

@
InterpreterF [f,g] a
@

-}
data InterpreterF effects a = InterpreterF { getInterpreterF ::
 Rec (Apply a) effects
 }

--------------------------------------------------------------------------------
