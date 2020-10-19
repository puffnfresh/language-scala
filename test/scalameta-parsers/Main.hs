{-
We compare the JSON to the version we parsed from the original file, to ensure
the pretty printer doesn't accidentally change syntax.
-}

module Main where

import Data.Aeson (eitherDecodeFileStrict)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Prettyprint.Doc (Pretty (pretty))
import Language.Scala (Source)
import Language.Scala.Parser.External (runNodeScalametaParsers)
import System.FilePath (dropExtension, takeBaseName, (</>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsStringDiff)

main :: IO ()
main =
  tests >>= defaultMain

scalametaParserTest :: FilePath -> TestTree
scalametaParserTest f =
  goldenVsStringDiff (dropExtension (takeBaseName f)) (\ref new -> ["diff", "-u", ref, new]) f $ do
    parsed <- eitherDecodeFileStrict f :: IO (Either String Source)
    either error ((encodeUtf8 . pack <$>) . runNodeScalametaParsers . show . pretty) parsed

tests :: IO TestTree
tests =
  testGroup "scalameta-parsers" . map scalametaParserTest <$> findByExtension [".json"] ("test" </> "fixtures")
