module Chez where

import Prelude

import Data.List (List)
import Data.Set (Set)
import Effect (Effect)

type FilePath = String

foreign import argv :: Effect (List String)
foreign import exit :: Int -> Effect Unit

-- FS
foreign import readUTF8TextFile :: FilePath -> Effect String

-- Glob
foreign import expandGlobs :: String -> Array String -> Effect (Set String)
