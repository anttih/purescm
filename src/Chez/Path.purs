module Chez.Path where

import Data.String as String

type FilePath = String

foreign import directorySeparator :: String

concat :: Array String -> String
concat parts = String.joinWith directorySeparator parts

foreign import extname :: FilePath -> String
