module Hilcode.Configuration
    ( Configuration(..)
    , Copyright(..)
    , AccessType(..)
    ) where

import Data.Text
    (Text)
import Prelude
    (Enum, Eq, Int, Show)

data Copyright = Copyright
    { year  :: Int
    , owner :: Text
    } deriving (Show)

data AccessType
    = USE_GETTERS
    | USE_FIELDS
    deriving (Enum, Eq, Show)

data Configuration = Configuration
    { copyright  :: Copyright
    , accessType :: AccessType
    } deriving (Show)
