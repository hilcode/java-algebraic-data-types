{-# LANGUAGE OverloadedStrings #-}

module Java
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
    , makeJavaImport
    , makeJavaStaticImport
    , package
    , packageName
    , type__int
    , type__java_lang_String
    , type__java_util_UUID
    , type__java_util_List
    , type__org_example_Xyz
    ) where

import Data.Text
    (Text, append, isPrefixOf)
import Prelude
    (Eq, Int, Ord)

type__int :: JavaType
type__int = JavaType "int" []
type__java_lang_String :: JavaType
type__java_lang_String = JavaType "String" []
type__java_util_UUID :: JavaType
type__java_util_UUID = JavaType "UUID" [makeJavaImport "java.util.UUID"]
type__java_util_List :: JavaType
type__java_util_List = JavaType "List" [makeJavaImport "java.util.List"]
type__org_example_Xyz :: JavaType
type__org_example_Xyz = JavaType "Xyz" [makeJavaImport "org.example.Xyz"]

newtype JavaPackage
    = JavaPackage Text

package :: Text -> JavaPackage
package = JavaPackage

data JavaImport
    = JavaImport Int Text
    | JavaStaticImport Int Text
    deriving (Eq, Ord)

data JavaClassName =
    JavaClassName JavaPackage
                  Text

newtype JavaFieldName =
    JavaFieldName Text

data JavaType =
    JavaType Text
             [JavaImport]

data JavaField =
    JavaField JavaFieldName
              JavaType

data JavaClass =
    JavaClass JavaClassName
              [JavaField]

fields :: JavaClass -> [JavaField]
fields (JavaClass _ fields') = fields'

packageName :: JavaClass -> Text
packageName (JavaClass (JavaClassName (JavaPackage packageName') _) _) = packageName'

className :: JavaClass -> Text
className (JavaClass (JavaClassName _ className') _) = className'

makeJavaImport :: Text -> JavaImport
makeJavaImport javaType = JavaImport (javaImportGroup javaType) javaType

makeJavaStaticImport :: Text -> JavaImport
makeJavaStaticImport javaType = JavaStaticImport (javaImportGroup javaType) javaType

javaImportGroup :: Text -> Int
javaImportGroup javaType =
    if isPrefixOf "java." javaType then 1
    else if isPrefixOf "javax." javaType then 2
    else if isPrefixOf "org." javaType then 3
    else if isPrefixOf "com." javaType then 4
    else 5

javaImportToText :: JavaImport -> Text
javaImportToText (JavaImport _ text)       = "import " `append` text `append` ";"
javaImportToText (JavaStaticImport _ text) = "import static " `append` text `append` ";"

{--

javaFile : {
    notice : {
        javaDoc : [ "", "" ]
        multiLineComment : [ "", "" ]
        singleLineComment : [ "", "" ]
    }
    class : {
        javaDoc : [ "", "" ],
        annotations : [
            { name : "", arguments : [ "", "" ] }
        ],
        staticFields : [
            { modifiers : [ "", "" ], name : "", type : 
        ],
        staticInitializer : {
        }
        fields : [
        ],
        initializer : {
        },
        staticFunctions : [
        ],
        constructors : [
        ],
        methods : [
        ],
        nestedClasses : [
        ],
        innerClasses : [
        ]
    }
    interface : {
        javaDoc : [ "", "" ],
        annotations : [
            { name : "", arguments : [ "", "" ] }
        ],
        fields : [
        ],
        functions : [
        ],
        defaultFunctions : [
        ],
        nestedClasses : [
        ],
        innerClasses : [
        ]
    }
    
}

--}
