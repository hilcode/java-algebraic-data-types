module Hilcode.Configuration
    ( Configuration(..)
    , Copyright(..)
    , AccessType(..)
    , JavaTemplate(..)
    , NoticeTemplate(..)
    , NoticeTemplatePart(..)
    ) where

import Data.Text
    (Text)
import Prelude
    (Enum, Eq, Int)

import Hilcode.Base
import Hilcode.Indentation

data Copyright = Copyright Int Text

instance HasCopyright Copyright where
    year  (Copyright year' _     ) = year'
    owner (Copyright _     owner') = owner'

data AccessType
    = USE_GETTERS
    | USE_FIELDS
    deriving (Enum, Eq)

data Configuration = Configuration
    { indentationStep :: IndentationStep
    , configCopyright :: Copyright
    , accessType      :: AccessType
    , template        :: JavaTemplate
    }

instance HasCopyright Configuration where
    year (Configuration _ copyright' _ _) = year copyright'
    owner (Configuration _ copyright' _ _) = owner copyright'

data JavaTemplate
    = JavaTemplate NoticeTemplate

data NoticeTemplatePart
    = Static Text
    | CopyrightYear
    | CopyrightName

data NoticeTemplate
    = NoticeTemplate [[NoticeTemplatePart]]
