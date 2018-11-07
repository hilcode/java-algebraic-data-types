module ToText
    ( ToText
    , toText
    , ToIndentedText
    , toIndentedText
    ) where

import Data.Text (Text)

import Indentation

class ToText a where
    toText :: a -> Text

class ToIndentedText a where
    toIndentedText :: IndentationStep -> a -> Text