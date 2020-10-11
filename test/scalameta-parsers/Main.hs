{-
Calls out to the scalameta-parsers npm library using node. The library parses
the pretty printed Scala code and dumps out the AST in JSON. We compare the JSON
to the version we parsed from the original file, to ensure the pretty printer
doesn't accidentally change syntax.
-}

module Main where

import Data.Aeson (eitherDecodeFileStrict)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Prettyprint.Doc (Pretty (pretty))
import Language.Scala (Source)
import System.Process (readProcess)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsStringDiff)
import System.FilePath ((</>))

main :: IO ()
main =
  tests >>= defaultMain

scalametaParserTest :: FilePath -> TestTree
scalametaParserTest f =
  goldenVsStringDiff f (\ref new -> ["diff", "-u", ref, new]) f $ do
    parsed <- eitherDecodeFileStrict f :: IO (Either String Source)
    let script = "test" </> "scalameta-parsers" </> "scalameta-parsers-to-json.js"
    either error ((encodeUtf8 . pack <$>) . readProcess "node" [script] . show . pretty) parsed

tests :: IO TestTree
tests =
  testGroup "scalameta-parsers" . map scalametaParserTest <$> findByExtension [".json"] ("test" </> "fixtures")
