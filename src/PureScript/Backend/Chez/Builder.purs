module PureScript.Backend.Chez.Builder where

import Prelude

import Chez (expandGlobs)
import Chez as FS
import Chez as Process
import Control.Monad.Cont.Trans (lift)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Compactable (separate)
import Data.Either (Either(..))
import Data.Foldable (foldl, for_, notElem)
import Data.Lazy as Lazy
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.Set.NonEmpty as NonEmptySet
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Uncurried (runEffectFn1)
import JSON as Json
import PureScript.Backend.Optimizer.Builder (BuildEnv, buildModules)
import PureScript.Backend.Optimizer.Convert (BackendModule, OptimizationSteps)
import PureScript.Backend.Optimizer.CoreFn (Ann, Module, ModuleName(..), Qualified(..))
import PureScript.Backend.Optimizer.CoreFn.Json (decodeModule, printJsonDecodeError)
import PureScript.Backend.Optimizer.CoreFn.Sort (emptyPull, pullResult, resumePull, sortModules)
import PureScript.Backend.Optimizer.Directives (parseDirectiveFile)
import PureScript.Backend.Optimizer.Directives.Defaults (defaultDirectives)
import PureScript.Backend.Optimizer.Semantics (InlineDirectiveMap)
import PureScript.Backend.Optimizer.Semantics.Foreign (coreForeignSemantics)
import PureScript.CST.Errors (printParseError)

type FilePath = String

basicBuildMain
  :: { coreFnDirectory :: FilePath
     , coreFnGlobs :: NonEmptyArray String
     , externalDirectivesFile :: Maybe FilePath
     , onCodegenModule :: BuildEnv -> Module Ann -> BackendModule -> OptimizationSteps -> Effect Unit
     , onPrepareModule :: BuildEnv -> Module Ann -> Effect (Module Ann)
     }
  -> Effect Unit
basicBuildMain options = do
  coreFnModulesFromOutput options.coreFnDirectory options.coreFnGlobs >>= case _ of
    Left errors -> do
      for_ errors \(Tuple filePath err) -> do
        Console.error $ filePath <> " " <> err
      liftEffect $ Process.exit 1
    Right coreFnModules -> do
      externalDirectives <- map (fromMaybe Map.empty) $ traverse externalDirectivesFromFile
        options.externalDirectivesFile
      coreFnModules # buildModules
        { directives: Map.union externalDirectives (parseDirectiveFile defaultDirectives).directives
        , foreignSemantics: coreForeignSemantics # Map.filterKeys \(Qualified mod _) -> mod
            `notElem`
              -- `Effect.Ref.modify` is implemented as a thread-safe operation
              -- in the `refs` package. Note that `Control.Monad.ST.Internal`
              -- will still be inlined.
              [ Just (ModuleName "Effect.Ref") ]
        , onCodegenModule: options.onCodegenModule
        , onPrepareModule: options.onPrepareModule
        , traceIdents: Set.empty
        , analyzeCustom: \_ _ -> Nothing 
        }

coreFnModulesFromOutput
  :: String
  -> NonEmptyArray String
  -> Effect (Either (NonEmptyArray (Tuple FilePath String)) (List (Module Ann)))
coreFnModulesFromOutput path globs = runExceptT do
  paths <- Set.toUnfoldable <$> lift
    (expandGlobs path ((_ <> "/corefn.json") <$> NonEmptyArray.toUnfoldable globs))
  case NonEmptyArray.toArray globs of
    [ "**" ] ->
      sortModules <$> modulesFromPaths paths
    _ ->
      go <<< foldl resumePull emptyPull =<< modulesFromPaths paths
  where
  modulesFromPaths paths = ExceptT do
    { left, right } <- separate <$> traverse readCoreFnModule paths
    pure $ maybe (Right right) Left $ NonEmptyArray.fromArray left

  pathFromModuleName (ModuleName mn) =
    path <> "/" <> mn <> "/corefn.json"

  go pull = case pullResult pull of
    Left needed ->
      go <<< foldl resumePull pull =<< modulesFromPaths
        (pathFromModuleName <$> NonEmptySet.toUnfoldable needed)
    Right modules ->
      pure $ Lazy.force modules

readCoreFnModule :: String -> Effect (Either (Tuple FilePath String) (Module Ann))
readCoreFnModule filePath = do
  contents <- runEffectFn1 FS.readUTF8TextFile filePath
  case lmap printJsonDecodeError <<< decodeModule =<< Json.parse contents of
    Left err -> do
      pure $ Left $ Tuple filePath err
    Right mod ->
      pure $ Right mod

externalDirectivesFromFile :: FilePath -> Effect InlineDirectiveMap
externalDirectivesFromFile filePath = do
  fileContent <- runEffectFn1 FS.readUTF8TextFile filePath
  let { errors, directives } = parseDirectiveFile fileContent
  for_ errors \(Tuple directive { position, error }) -> do
    Console.warn $ "Invalid directive [" <> show (position.line + 1) <> ":"
      <> show (position.column + 1)
      <> "]"
    Console.warn $ "  " <> directive
    Console.warn $ "  " <> printParseError error
  pure directives
