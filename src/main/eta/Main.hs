{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Reader
    (Reader, ask, mapReader, runReader)
import Data.List
    (groupBy, intercalate, map, sort)
import Data.Monoid
    ((<>))
import Data.Set
    (Set, null, toList)
import Data.Text
    (Text, append, concat, drop, pack, replace, take, toUpper)
import Data.Text.IO
    (putStr)
import Prelude
    (Bool(..), IO, otherwise, return, show, ($), (==))

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
    , package
    , packageName
    , type__int
    , type__java_lang_String
    , type__java_util_UUID
    )
import Line
    (Indentable(..), ToText(..), javaImportToText, tab)
import Misc
    (get)
import Template
    (LoopVar(..), Template(..), emptyLine, javaImports, join, line, lineWithImports, loop)

main :: IO ()
main = putStr $ replace (importsGoHereKey `append` "\n") importsAsText templateAsText
  where
    config = Configuration (Copyright 2018 "Me") USE_GETTERS
    point' = javaClassTemplate point
    importsAsText = runReader (mapReader javaImportsToText (javaImports point')) config
    templateAsText = runReader (toText tab point') config

importsGoHereKey :: Text
importsGoHereKey = "\0IMPORTS\0"

javaImportsToText :: Set JavaImport -> Text
javaImportsToText javaImports'
    | null javaImports' = ""
    | otherwise = joinJavaImports (listToText (sortJavaImports javaImports'))
      where
        sortJavaImports :: Set JavaImport -> [[JavaImport]]
        sortJavaImports javaImports'' = groupBy compareGroup (sort (toList javaImports''))
        compareGroup :: JavaImport -> JavaImport -> Bool
        compareGroup (JavaImport lftGroup _) (JavaImport rgtGroup _) = lftGroup == rgtGroup
        compareGroup (JavaStaticImport _ _) (JavaStaticImport _ _)   = True
        compareGroup _ _                                             = False
        listToText :: [[JavaImport]] -> [[Text]]
        listToText javaImports'' = map (map javaImportToText) javaImports''
        joinJavaImports :: [[Text]] -> Text
        joinJavaImports javaImports'' = "\n" `append` concat (intercalate ["\n"] javaImports'')

javaClassTemplate :: JavaClass -> Template
javaClassTemplate javaClass = join $ do
    configuration <- ask
    return $
        copyrightTemplate (configuration `get` copyright)       <>
        line ["package ", packageName javaClass, ";"]           <>
        line [importsGoHereKey]                                 <>
        emptyLine                                               <>
        line ["public final class ", className javaClass, " {"] <>
        indent (javaFieldsTemplate javaClass)                   <>
        indent (javaGettersTemplate javaClass)                  <>
        emptyLine                                               <>
        line ["}"]                                              <>
        emptyLine

copyrightTemplate :: Copyright -> Template
copyrightTemplate copyright' =
    line ["/**"]                                                                                 <>
    line [" * Copyright (C) ", pack (show (copyright' `get` year)), " ", copyright' `get` owner] <>
    line [" */"]

javaFieldsTemplate :: JavaClass -> Template
javaFieldsTemplate javaClass = loop javaFieldTemplate (fields javaClass)

javaFieldTemplate :: LoopVar -> JavaField -> Template
javaFieldTemplate _ javaField = join $ do
    javaFieldDeclaration <- fieldDeclaration javaField
    return $
        emptyLine                   <>
        line [javaFieldDeclaration]

fieldDeclaration :: JavaField -> Reader Configuration Text
fieldDeclaration (JavaField (JavaFieldName fieldName) (JavaType javaType _)) = do
    configuration <- ask
    return $
        case configuration `get` accessType of
            USE_GETTERS -> "private final " `append` javaType `append` " " `append` fieldName `append` ";"
            USE_FIELDS  -> "public final " `append` javaType `append` " " `append` fieldName `append` ";"

javaGettersTemplate :: JavaClass -> Template
javaGettersTemplate javaClass = join $ do
    configuration <- ask
    return $
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
toCamelCase name = (toUpper (take 1 name)) `append` (drop 1 name)

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
        ]
