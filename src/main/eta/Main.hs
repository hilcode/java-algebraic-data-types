{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative
    ((<$>))
import           Control.Monad
    ((>>=))
import qualified Control.Monad        as M
    (return)
import           Control.Monad.Reader
    (Reader)
import qualified Control.Monad.Reader as R
    (runReader)
import qualified Data.List            as L
    (foldr, groupBy, reverse, sort)
import           Data.Monoid
    ((<>))
import           Data.Set
    (Set)
import qualified Data.Set             as S
    (empty, null, toList)
import           Data.Text
    (Text)
import qualified Data.Text            as T
    (append, drop, pack, take, toUpper)
import qualified Data.Text.IO         as TIO
    (putStr)
import           Prelude
    (Bool(..), IO, otherwise, show, ($), (==))

import Configuration
    (AccessType(..), Configuration(..), Copyright(..))
import Java
    ( JavaClass(..)
    , JavaClassName(..)
    , JavaField(..)
    , JavaFieldName(..)
    , JavaImport(..)
    , JavaPackage(..)
    , JavaType(..)
    , className
    , fields
    , javaImportToText
    , package
    , packageName
    , type__int
    , type__java_lang_String
    , type__java_util_List
    , type__java_util_UUID
    , type__org_example_Xyz
    )
import Line
    (Indentable(..), Line(..), ToText(..), tab)
import Misc
    (get)
import Template
    (LoopVar(..), Template(..), emptyLine, line, lineWithImports, listOfImports, loop)

main :: IO ()
main = TIO.putStr pointWithImportsAsText
  where
    config :: Configuration
    config = Configuration (Copyright 2018 "Me") USE_GETTERS
    pointTemplate :: Template
    pointTemplate = javaClassTemplate point config
    pointWithImportsTemplate :: Template
    pointWithImportsTemplate = insertImports (javaImportsTemplate (R.runReader (extractJavaImports pointTemplate) config)) pointTemplate
    pointWithImportsAsText :: Text
    pointWithImportsAsText = R.runReader (toText tab pointWithImportsTemplate) config
    extractJavaImports :: Template -> Reader Configuration (Set JavaImport)
    extractJavaImports None                        = M.return S.empty
    extractJavaImports (Template javaImports' _)   = M.return javaImports'
    extractJavaImports (TemplateWithConfig reader) = reader >>= extractJavaImports

insertImports :: Template -> Template -> Template
insertImports importsTemplate template = insertImports' template
  where
    insertImports' :: Template -> Template
    insertImports' None                        = None
    insertImports' (TemplateWithConfig reader) = TemplateWithConfig $ insertImports' <$> reader
    insertImports' (Template _ lines)          = replaceImportsLineWithImports [] lines
    replaceImportsLineWithImports :: [Line] -> [Line] -> Template
    replaceImportsLineWithImports linesWithoutImports []     = Template S.empty (L.reverse linesWithoutImports)
    replaceImportsLineWithImports linesWithoutImports (line':lines) =
        case line' of
            Imports -> Template S.empty (L.reverse linesWithoutImports) <> importsTemplate <> Template S.empty lines
            _       -> replaceImportsLineWithImports (line':linesWithoutImports) lines

javaImportsTemplate :: Set JavaImport -> Template
javaImportsTemplate javaImports
    | S.null javaImports = None
    | otherwise          = L.foldr (<>) None (javaImportsToTemplate <$> sortedJavaImports)
      where
        javaImportsToTemplate :: [JavaImport] -> Template
        javaImportsToTemplate javaImportGroup =
            emptyLine <>
            (L.foldr (<>) None (javaImportToTemplate <$> javaImportGroup))
        sortedJavaImports :: [[JavaImport]]
        sortedJavaImports = L.groupBy compareGroup (L.sort (S.toList javaImports))
        compareGroup :: JavaImport -> JavaImport -> Bool
        compareGroup (JavaImport lftGroup _) (JavaImport rgtGroup _) = lftGroup == rgtGroup
        compareGroup (JavaStaticImport _ _) (JavaStaticImport _ _)   = True
        compareGroup _ _                                             = False
        javaImportToTemplate :: JavaImport -> Template
        javaImportToTemplate javaImport = line [javaImportToText javaImport]

javaClassTemplate :: JavaClass -> Configuration -> Template
javaClassTemplate javaClass configuration =
    copyrightTemplate (configuration `get` copyright)       <>
    line ["package ", packageName javaClass, ";"]           <>
    listOfImports                                           <>
    emptyLine                                               <>
    line ["public final class ", className javaClass, " {"] <>
    indent (javaFieldsTemplate javaClass configuration)     <>
    indent (javaGettersTemplate javaClass configuration)    <>
    emptyLine                                               <>
    line ["}"]                                              <>
    emptyLine

copyrightTemplate :: Copyright -> Template
copyrightTemplate copyright' =
    line ["/**"]                                                                                   <>
    line [" * Copyright (C) ", T.pack (show (copyright' `get` year)), " ", copyright' `get` owner] <>
    line [" */"]

javaFieldsTemplate :: JavaClass -> Configuration -> Template
javaFieldsTemplate javaClass configuration = loop (javaFieldTemplate configuration) (fields javaClass)

javaFieldTemplate :: Configuration -> LoopVar -> JavaField -> Template
javaFieldTemplate configuration _ javaField =
    let javaFieldDeclaration = fieldDeclaration javaField configuration
      in
        emptyLine                   <>
        line [javaFieldDeclaration]

fieldDeclaration :: JavaField -> Configuration -> Text
fieldDeclaration (JavaField (JavaFieldName fieldName) (JavaType javaType _)) configuration =
    case configuration `get` accessType of
        USE_GETTERS -> "private final " `T.append` javaType `T.append` " " `T.append` fieldName `T.append` ";"
        USE_FIELDS  -> "public final " `T.append` javaType `T.append` " " `T.append` fieldName `T.append` ";"

javaGettersTemplate :: JavaClass -> Configuration -> Template
javaGettersTemplate javaClass configuration =
    case configuration `get` accessType of
        USE_GETTERS -> loop javaGetterTemplate (fields javaClass)
        USE_FIELDS  -> None

javaGetterTemplate :: LoopVar -> JavaField -> Template
javaGetterTemplate _ (JavaField (JavaFieldName fieldName) (JavaType javaType imports)) =
    emptyLine                                                                            <>
    lineWithImports ["public ", javaType, " get", toCamelCase fieldName, "() {"] imports <>
    indent (line ["return this.", fieldName, ";"])                                       <>
    line ["}"]

toCamelCase :: Text -> Text
toCamelCase name = (T.toUpper (T.take 1 name)) `T.append` (T.drop 1 name)

org_example :: JavaPackage
org_example = package "org.example"

point :: JavaClass
point =
    JavaClass
        (JavaClassName org_example "Point")
        [ JavaField (JavaFieldName "x") type__int
        , JavaField (JavaFieldName "y") type__int
        , JavaField (JavaFieldName "name") type__java_lang_String
        , JavaField (JavaFieldName "uuid") type__java_util_UUID
        , JavaField (JavaFieldName "uuid2") type__java_util_UUID
        , JavaField (JavaFieldName "extra") type__java_util_List
        , JavaField (JavaFieldName "xyz") type__org_example_Xyz
        ]
