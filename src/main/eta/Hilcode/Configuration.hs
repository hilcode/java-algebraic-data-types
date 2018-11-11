module Hilcode.Configuration
    ( Configuration(..)
    , Copyright(..)
    , AccessType(..)
    , JavaTemplates(..)
    , NoticeTemplate(..)
    , NoticeTemplatePart(..)
    ) where

import Data.Text
    (Text)
import Prelude
    (Enum, Eq, Int)

data Copyright = Copyright
    { year  :: Int
    , owner :: Text
    }

data AccessType
    = USE_GETTERS
    | USE_FIELDS
    deriving (Enum, Eq)

data Configuration = Configuration
    { copyright  :: Copyright
    , accessType :: AccessType
    , templates  :: JavaTemplates
    }

data JavaTemplates
    = JavaTemplates NoticeTemplate

data NoticeTemplatePart
    = Static Text
    | CopyrightYear
    | CopyrightName

data NoticeTemplate
    = NoticeTemplate [[NoticeTemplatePart]]
