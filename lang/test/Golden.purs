module Test.Golden where

import Prelude

import Data.Either (either)
import Data.Foldable (intercalate)
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Run as Run
import Node.Encoding as Enc
import Node.FS.Aff (exists, readdir, readTextFile, writeTextFile)
import Node.Path as Path
import Test.Spec (Spec, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ do
  files <- readFiles "examples"
  evaled <- evalFiles files
  spec <- runTests evaled
  runSpec [consoleReporter] $ toUnit spec
  where
    evalFiles = traverse evalFile
    runTests = map sequence <<< traverse testFile
    -- `toUnit` is required because `spec` is `Spec (Array Unit)` and the second argument to `runSpec` is `Spec Unit`.
    toUnit spec = spec >>= const (pure unit)

type TestName = String

type GoldenFilePath = Path.FilePath

goldenVsString
  :: TestName
  -> GoldenFilePath
  -> String
  -> Aff (Spec Unit)
goldenVsString testName goldenFilePath str = do
  isFile <- exists goldenFilePath
  if isFile
    then do
      file <- readTextFile Enc.UTF8 goldenFilePath
      pure $ it testName $ shouldEqual file str
    else do
      _ <- writeTextFile Enc.UTF8 goldenFilePath str
      pure $ pending ("Wrote golden file to: " <> goldenFilePath)

testFile
  :: { path :: Path.FilePath, result :: String }
  -> Aff (Spec Unit)
testFile { path, result } = goldenVsString testName goldenPath result
  where
    testName = path <> " - " <> goldenPath
    goldenPath = Path.concat [ "golden-files"
                             , Path.basename path <> ".golden"
                             ]

evalFile
  :: { file :: String, path :: Path.FilePath }
  -> Aff { path :: Path.FilePath, result :: String }
evalFile { file, path } = pure $ { path: path, result: run file }
  where showErr = show
        showRes = intercalate "\n" <<< map show
        run = either showErr showRes <<< _.result <<< Run.runMany mempty

type DirName = String

readFiles
  :: DirName
  -> Aff (Array { file :: String, path :: Path.FilePath })
readFiles dirName = do
  fileNames <- readdir dirName
  let paths = map toPath fileNames
  traverse read paths
  where read path = do
          file <- readTextFile Enc.UTF8 path
          pure $ { file: file, path: path }
        toPath = \fileName -> Path.concat [dirName, fileName]
