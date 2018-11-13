{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Hilcode.ToText
    ( Line(..)
    , ToLines(..)
    , ToText(..)
    , ToIndentedText(..)
    ) where

import Data.Monoid((<>))
import Data.Text (Text)
import qualified Data.Text as Text

import Hilcode.Configuration
import Hilcode.Indentation

class ToText a where
    toText :: Configuration -> a -> Text

class ToIndentedText a where
    toIndentedText :: Configuration -> IndentationStep -> a -> Text

data Line
    = Line Int Text

instance ToText Line where
    toText config (Line indentationLevel text)
        = indentation' <> text
          where
            indentationStep' :: IndentationStep
            indentationStep' = indentationStep config
            indentation' :: Text
            indentation' = Text.replicate indentationLevel (indentation indentationStep')

instance ToText [Line] where
    toText config lines'
        = Text.intercalate "\n" (map (toText config) lines')

instance ToIndentedText Line where
    toIndentedText _ indentationStep' (Line indentationLevel line)
        = indent <> line <> "\n"
          where
            indent = Text.replicate indentationLevel (indentation indentationStep')

class ToLines a where
    toLines :: Configuration -> Int -> a -> [Line]
