{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Template
    ( LoopVar(..)
    , Template(..)
    , emptyLine
    , emptyLineIf
    , emptyLineUnless
    , javaImports
    , line
    , lineWithImports
    , lineIf
    , lineWithImportsIf
    , lineUnless
    , lineWithImportsUnless
    , loop
    ) where

import Control.Applicative
    (liftA2)
import Control.Monad.Reader
    (Reader, mapReader, return)
import Data.Set
    (Set, empty, fromList, union)
import Data.Text
    (Text, concat)
import Prelude
    (Bool(False, True), Int, Monoid, foldl, length, map, mappend, mempty, not, traverse, zip, (+), (++), (==), (>>=))

import Configuration
    (Configuration(..))
import Java
    (JavaImport)
import Line
    (Indentable(..), Line, ToText(..), imports, makeEmptyLine, toLine, toLineWithImports)

data Template
    = None
    | One Line
    | Many [Line]
    | Template (Reader Configuration Template)

instance Monoid Template where
    mempty = None
    mappend None content                              = content
    mappend content None                              = content
    mappend (One lftLine) (One rgtLine)               = Many [lftLine, rgtLine]
    mappend (One lftLine) (Many lines)                = Many (lftLine:lines)
    mappend one@(One _) (Template reader)             = Template (mapReader (one `mappend`) reader)
    mappend (Many lines) (One rgtLine)                = Many (lines ++ [rgtLine])
    mappend (Many lftLines) (Many rgtLines)           = Many (lftLines ++ rgtLines)
    mappend many@(Many _) (Template reader)           = Template (mapReader (many `mappend`) reader)
    mappend (Template reader) one@(One _)             = Template (mapReader (`mappend` one) reader)
    mappend (Template reader) many@(Many _)           = Template (mapReader (`mappend` many) reader)
    mappend (Template lftReader) (Template rgtReader) = Template (liftA2 mappend lftReader rgtReader)

instance Indentable Template where
    indent None              = None
    indent (One line')       = One (indent line')
    indent (Many lines)      = Many (map indent lines)
    indent (Template reader) = Template (mapReader indent reader)

instance ToText Template where
    toText _ None                       = return ""
    toText indentStep (One line')       = toText indentStep line'
    toText indentStep (Many lines)      = mapReader concat (traverse (toText indentStep) lines)
    toText indentStep (Template reader) = reader >>= toText indentStep

data LoopVar = LoopVar
    { index :: Int
    , first :: Bool
    , last  :: Bool
    , count :: Int
    }

loop :: forall model. (LoopVar -> model -> Template) -> [model] -> Template
loop template models = foldl (mappend) None contents
  where
    count' :: Int
    count' = length models
    loopVar :: Int -> LoopVar
    loopVar index' = LoopVar index' (index' == 0) (index' + 1 == count') count'
    contents :: [Template]
    contents = [template (loopVar index') model' | (index', model') <- zip [0 ..] models]

emptyLine :: Template
emptyLine = One makeEmptyLine

emptyLineIf :: Bool -> Template
emptyLineIf False = None
emptyLineIf True  = One makeEmptyLine

emptyLineUnless :: Bool -> Template
emptyLineUnless test = emptyLineIf (not test)

line :: [Text] -> Template
line lineParts = One (toLine lineParts)

lineWithImports :: [Text] -> [JavaImport] -> Template
lineWithImports lineParts imports' = One (toLineWithImports lineParts imports')

lineIf :: Bool -> [Text] -> Template
lineIf False _    = None
lineIf True texts = line texts

lineWithImportsIf :: Bool -> [Text] -> [JavaImport] -> Template
lineWithImportsIf False _ _           = None
lineWithImportsIf True texts imports' = lineWithImports texts imports'

lineUnless :: Bool -> [Text] -> Template
lineUnless test = lineIf (not test)

lineWithImportsUnless :: Bool -> [Text] -> [JavaImport] -> Template
lineWithImportsUnless test = lineWithImportsIf (not test)

javaImports :: Template -> Reader Configuration (Set JavaImport)
javaImports template =
    mapReader extractJavaImportsFromLines (extractLines template)

extractLines :: Template -> Reader Configuration [Line]
extractLines template = case template of
    None               -> return ([])
    One line'          -> return ([line'])
    Many lines         -> return lines
    Template template' -> template' >>= extractLines

extractJavaImportsFromLines :: [Line] -> Set JavaImport
extractJavaImportsFromLines []            = empty
extractJavaImportsFromLines (line':lines) = fromList (imports line') `union` extractJavaImportsFromLines lines
