{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Line
    ( Indentable(..)
    , Line(..)
    , ToText(..)
    , makeEmptyLine
    , makeImportsLine
    , makeIndentStep
    , spaces_2
    , spaces_4
    , spaces_8
    , tab
    , toLine
    ) where

import Control.Monad
    (return)
import Control.Monad.Reader
    (Reader)
import Data.Functor
    ((<$>))
import Data.Text
    (Text, append, concat, replicate, singleton)
import Data.Traversable
    (sequence)
import Prelude
    (Int, ($), (+))

class ToLine a where
    toLine :: a -> Line

newtype IndentStep =
    IndentStep Text

makeIndentStep :: Text -> IndentStep
makeIndentStep = IndentStep

tab :: IndentStep
tab = IndentStep "\t"

spaces_2 :: IndentStep
spaces_2 = IndentStep "  "

spaces_4 :: IndentStep
spaces_4 = IndentStep "    "

spaces_8 :: IndentStep
spaces_8 = IndentStep "        "

class Indentable a where
    indent :: a -> a

class ToText config a where
    toText :: IndentStep -> a -> Reader config Text

data Line
    = Imports
    | Line Int Text

instance Indentable Line where
    indent Imports                 = Imports
    indent (Line indentLevel text) = Line (indentLevel + 1) text

instance Indentable [Line] where
    indent lines = indent <$> lines

instance ToText config Line where
    toText _ Imports = return "IMPORTS SHOULD GO HERE"
    toText (IndentStep indentStep) (Line indentLevel text) =
        return $ replicate indentLevel indentStep `append` text `append` singleton '\n'

instance ToText config [Line] where
    toText indentStep lines = concat <$> sequence ((toText indentStep) <$> lines)

makeImportsLine :: Line
makeImportsLine = Imports

makeEmptyLine :: Line
makeEmptyLine = Line 0 ""

instance ToLine Text where
    toLine text = Line 0 text

instance ToLine [Text] where
    toLine texts = Line 0 (concat texts)
