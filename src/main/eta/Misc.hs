module Misc
    ( get
    ) where

import Prelude
    ()

get :: a -> (a -> b) -> b
get x f = f x
