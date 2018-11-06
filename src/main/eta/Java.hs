{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Java
    ( JavaFile(..)
    ) where

import Prelude (Int, Eq, Ord, map, show, (++))

import Data.Maybe (Maybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text

data IndentationStep
    = Tab
    | TwoSpaces
    | ThreeSpaces
    | FourSpaces
    | EightSpaces

indentation :: IndentationStep -> Text
indentation Tab         = "\t"
indentation TwoSpaces   = "  "
indentation ThreeSpaces = "   "
indentation FourSpaces  = "    "
indentation EightSpaces = "        "

class ToText a where
    toText :: a -> Text

class ToIndentedText a where
    toIndentedText :: IndentationStep -> a -> Text

data Line
    = Line Int Text

instance ToIndentedText Line where
    toIndentedText indentationStep (Line indentationLevel line) = indent <> line <> "\n"
      where
        indent = Text.replicate indentationLevel (indentation indentationStep)

class ToLines a where
    toLines :: Int -> a -> [Line]

newtype Year
    = Year Int
        deriving (Eq, Ord)

instance ToText Year where
    toText (Year year) = Text.pack (show year)

data Notice
    = Notice Text Year

instance ToLines Notice where
    toLines _ (Notice name year)
      = [ Line 0 "/*"
        , Line 0 (" * Copyright (C) " <> (toText year))
        , Line 0 (" * " <> name)
        , Line 0 " */" ]

newtype Package
    = Package Text

instance ToText Package where
    toText (Package package) = package

instance ToLines Package where
    toLines _ (Package package)
      = [ Line 0 ("package " <> package <> ";") ]

newtype ClassName
    = ClassName Text

instance ToText ClassName where
    toText (ClassName className) = className

newtype FunctionName
    = FunctionName Text

instance ToText FunctionName where
    toText (FunctionName functionName) = functionName

data Import
    = Import Package ClassName
    | StaticImport Package ClassName FunctionName

instance ToLines Import where
    toLines _ (Import package className) = [ Line 0 ("import " <> (toText package) <> "." <> (toText className) <> ";") ]
    toLines _ (StaticImport package className functionName) = [ Line 0 ("import static " <> (toText package) <> "." <> (toText className) <> "." <> (toText functionName) <> ";") ]

newtype JavaDoc
    = JavaDoc [Text]

instance ToLines [Text] where
    toLines _ [] = []
    toLines indentationLevel (line:lines) = [Line indentationLevel line] ++ (toLines indentationLevel lines)

instance ToLines JavaDoc where
    toLines indentationLevel (JavaDoc lines)
      = [ Line indentationLevel "/**" ] ++ (toLines indentationLevel lines') ++ [ Line indentationLevel " */" ]
        where
          lines' :: [Text]
          lines' = map (" * " <>) lines

newtype AnnotationName
    = AnnotationName Text

instance ToText AnnotationName where
    toText (AnnotationName annotationName) = annotationName

newtype AnnotationParameter
    = AnnotationParameter Text

instance ToText AnnotationParameter where
    toText (AnnotationParameter annotationParameter) = annotationParameter

data Annotation
    = Annotation AnnotationName [AnnotationParameter]

data Scope
    = Public
    | Protected
    | PackagePrivate
    | Private

data Final
    = NotFinal
    | Final

data ClassModifiers
    = NoClassModifiers
    | AbstractClass
    | FinalClass
    | StaticClass
    | StaticAbstractClass
    | StaticFinalClass

data InterfaceModifiers
    = NoInterfaceModifiers
    | StaticInterface

data PrimitiveType
    = Bool
    | Byte
    | Char
    | Short
    | Int
    | Long
    | Float
    | Double

newtype TypeName
    = TypeName Text

data SimpleType
    = SimpleType Package TypeName

data GenericType
    = GenericType SimpleType [GenericType]

data Type
    = Primitive PrimitiveType
    | Simple SimpleType
    | Generic GenericType

newtype Expression
    = Expression Text

data Clause
    = Clause Expression Block

data Statement
    = SingleLine Text
    | IfThen Clause
    | IfThenElse Clause Clause
    | If Clause [Clause] Clause
    | While Clause
    | DoWhile Clause

data Block
    = Block [Statement]

newtype FieldName
    = FieldName Text

data InstanceField
    = InstanceField Scope Final FieldName Type

data ClassField
    = ClassField Scope Final FieldName Type

newtype ParameterName
    = ParameterName Text

data Parameter
    = Parameter Final ParameterName Type

data Function
    = Function JavaDoc [Annotation] Scope Final FunctionName [Parameter] Block

data Method
    = AbstractMethod JavaDoc [Annotation] Scope FunctionName [Parameter]
    | Method JavaDoc [Annotation] Scope Final FunctionName [Parameter] Block

data Constructor
    = Constructor JavaDoc [Annotation] Scope FunctionName [Parameter] Block

newtype StaticInitializer
    = StaticInitializer Block

newtype Initializer
    = Initializer Block

data Class
    = Class JavaDoc [Annotation] Scope ClassModifiers ClassName Type [Type] [ClassField] [InstanceField] (Maybe StaticInitializer) [Function] (Maybe Initializer) [Constructor] [Method] [InnerClass] [NestedClass]

data Interface
    = Interface JavaDoc [Annotation] Scope InterfaceModifiers ClassName [Type] [ClassField] [Function] [NestedClass]

data InnerClass
    = InnerClass Class

data NestedClass
    = NestedClass Class
    | NestedInterface Interface

data JavaFile
    = TopLevelClass Notice Package [Import] Class [InnerClass] [NestedClass]
    | TopLevelInterface Notice Package [Import] Interface [NestedClass]
