module Chez where

import Prelude

import Data.List (List)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1)

type FilePath = String

foreign import argv :: Effect (List String)
foreign import exit :: Int -> Effect Unit

-- FS
foreign import readUTF8TextFile :: EffectFn1 FilePath String

foreign import writeUTF8TextFile :: EffectFn2 FilePath String String

foreign import copyFile :: EffectFn2 FilePath FilePath Unit

--
-- Globbing
--

expandGlobs :: String -> List String -> Effect (Set String)
expandGlobs path globs = do
  results <- for globs \g -> runEffectFn1 glob (path <> "/" <> g)
  pure $
    join results
    # Set.fromFoldable

foreign import glob :: EffectFn1 String (List String)


--
-- Subprocess communication
--

foreign import system :: EffectFn1 String Int
