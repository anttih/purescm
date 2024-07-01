module Main where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Chez as Chez
import Chez as FS
import Chez.Path as Path
import Control.Monad.Error.Class (try)
import Data.Array as Array
import Data.Either (Either(..), isRight)
import Data.List as List
import Data.Maybe (Maybe, fromMaybe)
import Data.Monoid (power)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Dodo (plainText)
import Dodo as Dodo
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (message)
import Effect.Uncurried (runEffectFn1, runEffectFn2)
import PureScript.Backend.Chez.Builder (basicBuildMain)
import PureScript.Backend.Chez.Constants (moduleLib, moduleForeign, schemeExt)
import PureScript.Backend.Chez.Convert (codegenModule)
import PureScript.Backend.Chez.Printer as Printer
import PureScript.Backend.Optimizer.CoreFn (Module(..), ModuleName(..))
import Spago.Generated.BuildInfo as BuildInfo

type FilePath = String

type BuildArgs =
  { coreFnDir :: FilePath
  , outputDir :: FilePath
  , directivesFile :: Maybe FilePath
  }

data Command
  = Build BuildArgs

cliArgParser :: ArgParser Command
cliArgParser =
  ArgParser.choose "command"
    [ ArgParser.command [ "build" ]
        "Builds Chez scheme code from corefn.json files"
        do
          Build <$> buildCmdArgParser <* ArgParser.flagHelp
    ]
    <* ArgParser.flagHelp
    <* ArgParser.flagInfo [ "--version", "-v" ] "Show the current version of purescm."
      BuildInfo.packages.purescm

buildCmdArgParser :: ArgParser BuildArgs
buildCmdArgParser =
  ArgParser.fromRecord
    { coreFnDir:
        ArgParser.argument [ "--corefn-dir" ]
          "Path to input directory containing corefn.json files.\n\
          \Defaults to './output'."
          # ArgParser.default "./output"
    , outputDir:
        ArgParser.argument [ "--output" ]
          "Path to output directory for backend files.\n\
          \Defaults to './output'."
          # ArgParser.default "./output"
    , directivesFile:
        ArgParser.argument [ "--directives" ]
          "Path to file that defines external inline directives (optional)."
          # ArgParser.optional
    }

main :: Effect Unit
main = do
  let cliArgs = List.singleton "build"
  -- cliArgs <- List.drop 2 <$> Chez.argv
  case
    ArgParser.parseArgs "purescm" "Chez Scheme backend for PureScript" cliArgParser
      cliArgs
    of
    Left err ->
      Console.error $ ArgParser.printArgError err
    Right (Build args) -> do
      try (runBuild args) >>= case _ of
        Right _ -> pure unit
        Left err -> do
          Console.error (message err)
          Chez.exit 1

runBuild :: BuildArgs -> Effect Unit
runBuild args = do
  let runtimePath = Path.concat [ args.outputDir, "purs", "runtime" ]
  mkdirp runtimePath
  basicBuildMain
    { coreFnDirectory: args.coreFnDir
    , coreFnGlobs: pure "**"
    , externalDirectivesFile: args.directivesFile
    , onCodegenModule: \_ (Module { name: ModuleName name, path }) backend _ -> do
        let
          formatted =
            Dodo.print plainText Dodo.twoSpaces
              $ Printer.printLibrary
              $ codegenModule backend
        let modPath = Path.concat [ args.outputDir, name ]
        mkdirp modPath
        let libPath = Path.concat [ modPath, moduleLib <> schemeExt ]
        void $ runEffectFn2 FS.writeUTF8TextFile libPath formatted
        unless (Set.isEmpty backend.foreign) do
          let
            foreignSiblingPath =
              fromMaybe path (String.stripSuffix (Pattern (Path.extname path)) path) <>
                schemeExt
          let foreignOutputPath = Path.concat [ modPath, moduleForeign <> schemeExt ]
          res <- try $ runEffectFn2 FS.copyFile foreignSiblingPath foreignOutputPath
          unless (isRight res) do
            Console.log $ "  Foreign implementation missing."
    , onPrepareModule: \build coreFnMod@(Module { name }) -> do
        let total = show build.moduleCount
        let index = show (build.moduleIndex + 1)
        let padding = power " " (SCU.length total - SCU.length index)
        Console.log $ Array.fold
          [ "[", padding, index, " of ", total, "] purescm: building ", unwrap name ]
        pure coreFnMod
    }

mkdirp :: FilePath -> Effect Unit
mkdirp path = void $ runEffectFn1 Chez.system $ "mkdir -p " <> path

