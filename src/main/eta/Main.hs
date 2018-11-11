{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import Data.List (foldl')
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text

import Hilcode.Configuration
import Hilcode.Java
import Hilcode.ToText

main :: IO ()
main = putStrLn "Hello"

noticeTemplate :: NoticeTemplate
noticeTemplate = NoticeTemplate
    [ [Static "/*"]
    , [Static " * Copyright (C) ", CopyrightYear, Static " ", CopyrightName]
    , [Static " */"]
    ]

class ToLines2 config a where
    toLines2 :: config -> a -> [Line]

instance ToLines2 Configuration NoticeTemplate where
    toLines2 config (NoticeTemplate lineParts)
        = map x lineParts
          where
            x :: [NoticeTemplatePart] -> Line
            x parts = Line 0 (foldl' z "" parts)
            z :: Text -> NoticeTemplatePart -> Text
            z resultSoFar (Static text) = resultSoFar <> text
            z resultSoFar CopyrightYear = resultSoFar <> (Text.pack $ show copyrightYear)
            z resultSoFar CopyrightName = resultSoFar <> copyrightName
            copyright' :: Copyright
            copyright' = copyright config
            copyrightYear :: Int
            copyrightYear = year copyright'
            copyrightName :: Text
            copyrightName = owner copyright'
