{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

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
    , listOfImports
    , loop
    ) where

import Control.Applicative
    (liftA2)
import Control.Monad
    (return, (>>=))
import Control.Monad.Reader
    (Reader)
import Data.Functor
    ((<$>))
import Data.Set
    (Set, empty, fromList)
import Data.Text
    (Text)
import Prelude
    (Bool(False, True), Int, Monoid, foldl, length, mappend, mempty, not, zip, ($), (+), (==))

import Configuration
    (Configuration)
import Java
    (JavaImport)
import Line
    (Indentable(..), Line, ToText(..), makeEmptyLine, makeImportsLine, toLine)

data Template
    = None
    | Template (Set JavaImport) [Line]
    | TemplateWithConfig (Reader Configuration Template)

instance Monoid Template where
    mempty = None
    mappend None template                                                         = template
    mappend template None                                                         = template
    mappend (Template lftJavaImports lftLines) (Template rgtJavaImports rgtLines) = Template (lftJavaImports `mappend` rgtJavaImports) (lftLines `mappend` rgtLines)
    mappend template@(Template _ _)            (TemplateWithConfig reader)        = TemplateWithConfig $ (template `mappend`) <$> reader
    mappend (TemplateWithConfig reader)        template@(Template _ _)            = TemplateWithConfig $ (`mappend` template) <$> reader
    mappend (TemplateWithConfig lftReader)     (TemplateWithConfig rgtReader)     = TemplateWithConfig $ liftA2 mappend lftReader rgtReader

instance Indentable Template where
    indent None                          = None
    indent (Template javaImports' lines) = Template javaImports' (indent <$> lines)
    indent (TemplateWithConfig reader)   = TemplateWithConfig $ indent <$> reader

instance ToText Configuration Template where
    toText _ None                                 = return ""
    toText indentStep (Template _ lines)          = toText indentStep lines
    toText indentStep (TemplateWithConfig reader) = reader >>= toText indentStep

data LoopVar = LoopVar
    { index :: Int
    , first :: Bool
    , last  :: Bool
    , count :: Int
    }

loop :: forall model. (LoopVar -> model -> Template) -> [model] -> Template
loop template models = foldl (mappend) None templates
  where
    modelCount :: Int
    modelCount = length models
    loopVar :: Int -> LoopVar
    loopVar i = LoopVar i (i == 0) (i + 1 == modelCount) modelCount
    templates :: [Template]
    templates = [template (loopVar i) model | (i, model) <- zip [0 ..] models]

listOfImports :: Template
listOfImports = Template empty [makeImportsLine]

emptyLine :: Template
emptyLine = Template empty [makeEmptyLine]

emptyLineIf :: Bool -> Template
emptyLineIf False = None
emptyLineIf True  = Template empty [makeEmptyLine]

emptyLineUnless :: Bool -> Template
emptyLineUnless test = emptyLineIf (not test)

line :: [Text] -> Template
line lineParts = Template empty [toLine lineParts]

lineWithImports :: [Text] -> [JavaImport] -> Template
lineWithImports lineParts imports = Template (fromList imports) [toLine lineParts]

lineIf :: Bool -> [Text] -> Template
lineIf False _    = None
lineIf True texts = line texts

lineWithImportsIf :: Bool -> [Text] -> [JavaImport] -> Template
lineWithImportsIf False _ _           = None
lineWithImportsIf True texts imports = lineWithImports texts imports

lineUnless :: Bool -> [Text] -> Template
lineUnless test = lineIf (not test)

lineWithImportsUnless :: Bool -> [Text] -> [JavaImport] -> Template
lineWithImportsUnless test = lineWithImportsIf (not test)

javaImports :: Template -> Reader Configuration (Set JavaImport)
javaImports None                        = return empty
javaImports (Template imports _)        = return imports
javaImports (TemplateWithConfig reader) = reader >>= javaImports
