{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Hilcode.Java where

import Prelude (Int, Eq, Ord, map, show, (++))

import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text

import Hilcode.Configuration
import Hilcode.Indentation
import Hilcode.ToText

newtype Year
    = Year Int
        deriving (Eq, Ord)

instance ToText Year where
    toText _ (Year year') = Text.pack (show year')

data Notice
    = Notice Text Year

newtype Package
    = Package Text

instance ToText Package where
    toText _ (Package package) = package

instance ToLines Package where
    toLines _ _ (Package package)
      = [ Line 0 ("package " <> package <> ";") ]

newtype ClassName
    = ClassName Text

instance ToText ClassName where
    toText _ (ClassName className) = className

newtype FunctionName
    = FunctionName Text

instance ToText FunctionName where
    toText _ (FunctionName functionName) = functionName

data Import
    = Import Package ClassName
    | StaticImport Package ClassName FunctionName

instance ToLines Import where
    toLines config _ (Import package className)
        = [ Line 0 ("import " <> (toText config package) <> "." <> (toText config className) <> ";") ]
    toLines config _ (StaticImport package className functionName)
        = [ Line 0 ("import static " <> (toText config package) <> "." <> (toText config className) <> "." <> (toText config functionName) <> ";") ]

newtype JavaDoc
    = JavaDoc [Text]

instance ToLines [Text] where
    toLines _ _ [] = []
    toLines config indentationLevel (line:lines) = [Line indentationLevel line] ++ (toLines config indentationLevel lines)

instance ToLines JavaDoc where
    toLines config indentationLevel (JavaDoc lines)
      = [ Line indentationLevel "/**" ] ++ (toLines config indentationLevel lines') ++ [ Line indentationLevel " */" ]
        where
          lines' :: [Text]
          lines' = map (" * " <>) lines

newtype AnnotationName
    = AnnotationName Text

instance ToText AnnotationName where
    toText _ (AnnotationName annotationName) = annotationName

newtype AnnotationParameter
    = AnnotationParameter Text

instance ToText AnnotationParameter where
    toText _ (AnnotationParameter annotationParameter) = annotationParameter

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
    | Switch Expression [Clause] (Maybe Clause)

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
    = TopLevelClass (Maybe Notice) Package [Import] Class [InnerClass] [NestedClass]
    | TopLevelInterface (Maybe Notice) Package [Import] Interface [NestedClass]

class JavaBuilder a where
    toJava :: Configuration -> a -> [Line]

instance JavaBuilder (Maybe Notice) where
    toJava _ Nothing
        = []
    toJava config (Just (Notice name year'))
        = [ Line 0  "/*"
          , Line 0 (" * Copyright (C) " <> (toText config year') <> " " <> name)
          , Line 0  " */" ]

instance JavaBuilder JavaFile where
    toJava config (TopLevelClass notice package imports class' innerClasses nestedClasses)
        = toJava config notice
    toJava config (TopLevelInterface notice package imports interface nestedClasses)
        = toJava config notice
