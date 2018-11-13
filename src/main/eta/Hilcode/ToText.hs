module Hilcode.ToText
    ( ToText(..)
    , ToIndentedText(..)
    ) where

import Data.Text (Text)

import Hilcode.Configuration
import Hilcode.Indentation

class ToText a where
    toText :: Configuration -> a -> Text

class ToIndentedText a where
    toIndentedText :: Configuration -> IndentationStep -> a -> Text
