{-# LANGUAGE MultiParamTypeClasses #-}

module Hilcode.Base
    ( HasConfiguration(..)
    , HasCopyright(..)
    ) where

import Data.Text
    (Text)

import Hilcode.Indentation

class HasCopyright a where
    year :: a -> Int
    owner :: a -> Text

class HasCopyright copyrightType => HasConfiguration configurationType copyrightType where
    indentationStep :: configurationType -> IndentationStep
    copyright :: configurationType -> copyrightType
