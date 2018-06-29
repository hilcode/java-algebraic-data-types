{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Line
    ( Indentable(..)
    , Line
    , ToText(..)
    , imports
    , javaImportToText
    , makeEmptyLine
    , makeIndentStep
    , spaces_2
    , spaces_4
    , spaces_8
    , tab
    , toLine
    , toLineWithImports
    ) where

import Control.Monad.Reader
    (Reader)
import Data.Text
    (Text, append, concat, replicate, singleton)
import Prelude
    (Int, return, ($), (+))

import Configuration
    (Configuration)
import Java
    (JavaImport(..))

class ToLine a where
    toLine            :: a                 -> Line
    toLineWithImports :: a -> [JavaImport] -> Line

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

class ToText a where
    toText :: IndentStep -> a -> Reader Configuration Text

data Line =
    Line Int
         Text
         [JavaImport]

instance Indentable Line where
    indent (Line indentLevel text imports') =
        Line (indentLevel + 1) text imports'

instance ToText Line where
    toText (IndentStep indentStep) (Line indentLevel text _) =
        return $
            replicate indentLevel indentStep `append` text `append` singleton '\n'

makeEmptyLine :: Line
makeEmptyLine = Line 0 "" []

instance ToLine Text where
    toLine            text          = Line 0 text []
    toLineWithImports text imports' = Line 0 text imports'

instance ToLine [Text] where
    toLine            texts          = Line 0 (concat texts) []
    toLineWithImports texts imports' = Line 0 (concat texts) imports'

imports :: Line -> [JavaImport]
imports (Line _ _ javaImports) = javaImports

javaImportToText :: JavaImport -> Text
javaImportToText (JavaImport _ text)       = "import " `append` text `append` ";\n"
javaImportToText (JavaStaticImport _ text) = "import static " `append` text `append` ";\n"
