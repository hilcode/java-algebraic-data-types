{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Template
    ( LoopVar(..)
    , Template(..)
    , emptyLine
    , emptyLineIf
    , emptyLineUnless
    , javaImports
    , join
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
    (Text)
import Prelude
    (Bool(False, True), Int, Monoid, foldl, length, mappend, mempty, not, zip, ($), (+), (==), (>>=))

import Configuration
    (Configuration(..))
import Java
    (JavaImport)
import Line
    (Indentable(..), Line, ToText(..), imports, makeEmptyLine, toLine, toLineWithImports)

data Template
    = None
    | Template (Reader Configuration [Line])

instance Monoid Template where
    mempty = None
    mappend None template                             = template
    mappend template None                             = template
    mappend (Template lftReader) (Template rgtReader) = Template $ liftA2 mappend lftReader rgtReader

instance Indentable Template where
    indent None              = None
    indent (Template reader) = Template (mapReader indent reader)

instance ToText Template where
    toText _ None                       = return ""
    toText indentStep (Template reader) = reader >>= toText indentStep

data LoopVar = LoopVar
    { index :: Int
    , first :: Bool
    , last  :: Bool
    , count :: Int
    }

loop :: forall model. (LoopVar -> model -> Template) -> [model] -> Template
loop template models = foldl (mappend) None templates
  where
    count' :: Int
    count' = length models
    loopVar :: Int -> LoopVar
    loopVar index' = LoopVar index' (index' == 0) (index' + 1 == count') count'
    templates :: [Template]
    templates = [template (loopVar index') model' | (index', model') <- zip [0 ..] models]

emptyLine :: Template
emptyLine = Template $ return [makeEmptyLine]

emptyLineIf :: Bool -> Template
emptyLineIf False = None
emptyLineIf True  = Template $ return [makeEmptyLine]

emptyLineUnless :: Bool -> Template
emptyLineUnless test = emptyLineIf (not test)

line :: [Text] -> Template
line lineParts = Template $ return [toLine lineParts]

lineWithImports :: [Text] -> [JavaImport] -> Template
lineWithImports lineParts imports' = Template $ return [toLineWithImports lineParts imports']

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
    None               -> return []
    Template template' -> template'

extractJavaImportsFromLines :: [Line] -> Set JavaImport
extractJavaImportsFromLines []            = empty
extractJavaImportsFromLines (line':lines) = fromList (imports line') `union` extractJavaImportsFromLines lines

join :: Reader Configuration Template -> Template
join reader = Template $ reader >>= extract
  where
    extract :: Template -> Reader Configuration [Line]
    extract None               = return []
    extract (Template reader') = reader'
