module Main where

import Data.Aeson (eitherDecodeFileStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Prettyprint.Doc (LayoutOptions (LayoutOptions), PageWidth (AvailablePerLine), Pretty (pretty), layoutSmart)
import Data.Text.Prettyprint.Doc.Render.Text (renderLazy)
import Language.Scala (Source)
import System.FilePath (dropExtension, takeBaseName, (</>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsStringDiff)

main :: IO ()
main =
  tests >>= defaultMain

prettyTest :: FilePath -> TestTree
prettyTest f =
  goldenVsStringDiff (dropExtension (takeBaseName f)) (\ref new -> ["diff", "-u", ref, new]) (f ++ ".golden.scala") $ do
    parsed <- eitherDecodeFileStrict f :: IO (Either String Source)
    either error (pure . encodeUtf8 . renderLazy . layoutSmart (LayoutOptions (AvailablePerLine 150 1.0)) . pretty) parsed

tests :: IO TestTree
tests =
  testGroup "Golden" . map prettyTest <$> findByExtension [".json"] ("test" </> "fixtures")
