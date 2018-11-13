{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import Data.List (foldl')
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text

import Hilcode.Configuration
import Hilcode.Indentation
import Hilcode.Java
import Hilcode.ToText

main :: IO ()
main = putStrLn $ Text.unpack output
  where
    output :: Text
    output = toText config (toLines2 config noticeTemplate)
    config :: Configuration
    config = Configuration Tab (Copyright 2018 "Hilco Wijbenga") USE_FIELDS (JavaTemplates noticeTemplate)

noticeTemplate :: NoticeTemplate
noticeTemplate = NoticeTemplate
    [ [Static "Copyright (C) ", CopyrightYear, Static " ", CopyrightName]
    ]

class ToLines2 config a where
    toLines2 :: config -> a -> [Line]

instance HasCopyright configuration => ToLines2 configuration NoticeTemplate where
    toLines2 config (NoticeTemplate lineParts)
        = [Line 0 "/**"] <> map x lineParts <> [Line 0 " */"]
          where
            x :: [NoticeTemplatePart] -> Line
            x parts = Line 0 (foldl' z " * " parts)
            z :: Text -> NoticeTemplatePart -> Text
            z resultSoFar (Static text) = resultSoFar <> text
            z resultSoFar CopyrightYear = resultSoFar <> (Text.pack $ show copyrightYear)
            z resultSoFar CopyrightName = resultSoFar <> copyrightName
            copyrightYear :: Int
            copyrightYear = year config
            copyrightName :: Text
            copyrightName = owner config
