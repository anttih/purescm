module Main where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Chez as Chez
import Data.Either (Either(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console as Console
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

main :: FilePath -> Effect Unit
main cliRoot = do
  cliArgs <- List.drop 2 <$> Chez.argv
  case
    ArgParser.parseArgs "purescm" "Chez Scheme backend for PureScript" cliArgParser
      cliArgs
    of
    Left err ->
      Console.error $ ArgParser.printArgError err
    Right (Build args) ->
      Chez.exit 1
      -- runBuild args >>= case _ of
      --   Right _ -> pure unit
      --   Left err -> do
      --     Console.error (message err)
      --     Process.exit' 1

-- runBuild :: BuildArgs -> Aff Unit
-- runBuild args = do
--   let runtimePath = Path.concat [ args.outputDir, "purs", "runtime" ]
--   mkdirp runtimePath
--   basicBuildMain
--     { coreFnDirectory: args.coreFnDir
--     , coreFnGlobs: pure "**"
--     , externalDirectivesFile: args.directivesFile
--     , onCodegenModule: \_ (Module { name: ModuleName name, path }) backend _ -> do
--         let
--           formatted =
--             Dodo.print plainText Dodo.twoSpaces
--               $ Printer.printLibrary
--               $ codegenModule backend
--         let modPath = Path.concat [ args.outputDir, name ]
--         mkdirp modPath
--         let libPath = Path.concat [ modPath, moduleLib <> schemeExt ]
--         FS.writeTextFile UTF8 libPath formatted
--         unless (Set.isEmpty backend.foreign) do
--           let
--             foreignSiblingPath =
--               fromMaybe path (String.stripSuffix (Pattern (Path.extname path)) path) <>
--                 schemeExt
--           let foreignOutputPath = Path.concat [ modPath, moduleForeign <> schemeExt ]
--           res <- attempt $ copyFile foreignSiblingPath foreignOutputPath
--           unless (isRight res) do
--             Console.log $ "  Foreign implementation missing."
--     , onPrepareModule: \build coreFnMod@(Module { name }) -> do
--         let total = show build.moduleCount
--         let index = show (build.moduleIndex + 1)
--         let padding = power " " (SCU.length total - SCU.length index)
--         Console.log $ Array.fold
--           [ "[", padding, index, " of ", total, "] purescm: building ", unwrap name ]
--         pure coreFnMod
--     }

