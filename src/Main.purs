module Main where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Control.Monad.Except (ExceptT(..), lift, runExceptT)
import Control.Parallel (parTraverse)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Compactable (separate)
import Data.Either (Either(..))
import Data.Foldable (for_, foldl)
import Data.Lazy as Lazy
import Data.List (List)
import Data.Maybe (fromMaybe, maybe)
import Data.Monoid (power)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.Set.NonEmpty as NonEmptySet
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Dodo (plainText)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, error, launchAff_, makeAff, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.FS.Perms (mkPerms)
import Node.FS.Perms as Perms
import Node.FS.Stats as Stats
import Node.FS.Stream (createReadStream, createWriteStream)
import Node.Glob.Basic (expandGlobs)
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Node.Stream as Stream
import PureScript.Backend.Chez.Constants (moduleForeign, moduleLib, schemeExt)
import PureScript.Backend.Chez.Convert (codegenModule)
import PureScript.Backend.Chez.Printer as Printer
import PureScript.Backend.Optimizer.Builder (buildModules)
import PureScript.Backend.Optimizer.CoreFn (Ann, Module(..), ModuleName(..))
import PureScript.Backend.Optimizer.CoreFn.Json (decodeModule)
import PureScript.Backend.Optimizer.CoreFn.Sort (emptyPull, pullResult, resumePull, sortModules)
import PureScript.Backend.Optimizer.Directives (parseDirectiveFile)
import PureScript.Backend.Optimizer.Directives.Defaults (defaultDirectives)
import PureScript.Backend.Optimizer.Semantics.Foreign (coreForeignSemantics)

type BuildArgs =
  { coreFnDir :: FilePath
  , outputDir :: FilePath
  }

data Command = Build BuildArgs

cliArgParser :: ArgParser Command
cliArgParser =
  ArgParser.choose "command"
    [ ArgParser.command [ "build" ]
        "Builds Chez scheme code from corefn.json files"
        do
          Build <$> buildArgsParser <* ArgParser.flagHelp
    ]
    <* ArgParser.flagHelp

buildArgsParser :: ArgParser BuildArgs
buildArgsParser =
  ArgParser.fromRecord
    { coreFnDir:
        ArgParser.argument [ "--corefn-dir" ]
          "Path to input directory containing corefn.json files.\n\
          \Defaults to './output'."
          # ArgParser.default (Path.concat [ ".", "output" ])
    , outputDir:
        ArgParser.argument [ "--output-dir" ]
          "Path to output directory for backend files.\n\
          \Defaults to './output-chez'."
          # ArgParser.default (Path.concat [ ".", "output-chez" ])
    }

main :: Effect Unit
main = do
  cliArgs <- Array.drop 2 <$> Process.argv
  case ArgParser.parseArgs "purescm" "Chez scheme PureScript backend" cliArgParser cliArgs of
    Left err ->
      Console.error $ ArgParser.printArgError err
    Right (Build args) ->
      launchAff_ $ runBuild args

runBuild :: BuildArgs -> Aff Unit
runBuild args = do
  coreFnModulesFromOutput args.coreFnDir (pure "**") >>= case _ of
    Left errors -> do
      for_ errors \(Tuple filePath err) -> do
        Console.error $ filePath <> " " <> err
      liftEffect $ Process.exit 1
    Right coreFnModules -> do
      let { directives } = parseDirectiveFile defaultDirectives
      -- No runtime .ss files needed yet
      -- copyFile (Path.concat [ "..", "..", "runtime.js" ]) (Path.concat [ testOut, "runtime.js" ])
      mkdirp args.outputDir
      coreFnModules # buildModules
        { directives
        , foreignSemantics: coreForeignSemantics -- no chez scheme specific foreign semantics yet
        , onCodegenModule: \_ (Module { name: ModuleName name, path }) backend -> do
            let
              formatted =
                Dodo.print plainText Dodo.twoSpaces
                  $ Printer.printLibrary
                  $ codegenModule backend
            let modPath = Path.concat [ args.outputDir, name ]
            mkdirp modPath
            let libPath = Path.concat [ modPath, moduleLib <> schemeExt ]
            FS.writeTextFile UTF8 libPath formatted
            unless (Set.isEmpty backend.foreign) do
              let
                foreignSiblingPath =
                  fromMaybe path (String.stripSuffix (Pattern (Path.extname path)) path) <> schemeExt
              let foreignOutputPath = Path.concat [ modPath, moduleForeign <> schemeExt ]
              copyFile foreignSiblingPath foreignOutputPath
        , onPrepareModule: \build coreFnMod@(Module { name }) -> do
            let total = show build.moduleCount
            let index = show (build.moduleIndex + 1)
            let padding = power " " (SCU.length total - SCU.length index)
            Console.log $ "[" <> padding <> index <> " of " <> total <> "] purescm: building " <> unwrap name
            pure coreFnMod
        }

coreFnModulesFromOutput
  :: String
  -> NonEmptyArray String
  -> Aff (Either (NonEmptyArray (Tuple FilePath String)) (List (Module Ann)))
coreFnModulesFromOutput path globs = runExceptT do
  paths <- Set.toUnfoldable <$> lift
    (expandGlobs path ((_ <> "/corefn.json") <$> NonEmptyArray.toArray globs))
  case NonEmptyArray.toArray globs of
    [ "**" ] ->
      sortModules <$> modulesFromPaths paths
    _ ->
      go <<< foldl resumePull emptyPull =<< modulesFromPaths paths
  where
  modulesFromPaths paths = ExceptT do
    { left, right } <- separate <$> parTraverse readCoreFnModule paths
    pure $ maybe (Right right) Left $ NonEmptyArray.fromArray left

  pathFromModuleName (ModuleName mn) =
    path <> "/" <> mn <> "/corefn.json"

  go pull = case pullResult pull of
    Left needed ->
      go <<< foldl resumePull pull =<< modulesFromPaths
        (pathFromModuleName <$> NonEmptySet.toUnfoldable needed)
    Right modules ->
      pure $ Lazy.force modules

-- Utils

readCoreFnModule :: String -> Aff (Either (Tuple FilePath String) (Module Ann))
readCoreFnModule filePath = do
  contents <- FS.readTextFile UTF8 filePath
  case lmap Json.printJsonDecodeError <<< decodeModule =<< Json.jsonParser contents of
    Left err -> do
      pure $ Left $ Tuple filePath err
    Right mod ->
      pure $ Right mod

mkdirp :: FilePath -> Aff Unit
mkdirp path = FS.mkdir' path { recursive: true, mode: mkPerms Perms.all Perms.all Perms.all }

copyFile :: FilePath -> FilePath -> Aff Unit
copyFile from to = do
  stats <- FS.stat from
  unless (Stats.isFile stats) do
    throwError $ error $ "Not a file: " <> from
  makeAff \k -> do
    src <- createReadStream from
    dst <- createWriteStream to
    res <- Stream.pipe src dst
    Stream.onError src (k <<< Left)
    Stream.onError dst (k <<< Left)
    Stream.onError res (k <<< Left)
    Stream.onFinish res (k (Right unit))
    pure $ effectCanceler do
      Stream.destroy res
      Stream.destroy dst
      Stream.destroy src

