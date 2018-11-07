{-# LANGUAGE OverloadedStrings #-}

module Indentation
    ( IndentationStep
    , indentation
    ) where

import Data.Text (Text)
import qualified Data.Text as Text

data IndentationStep
    = Tab
    | Spaces Int

indentation :: IndentationStep -> Text
indentation Tab             = "\t"
indentation (Spaces spaces) = Text.replicate spaces " "
