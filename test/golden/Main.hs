module Main where

import Data.Aeson (eitherDecodeFileStrict)
import Language.Scala (Source)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff, findByExtension)
import Data.Text.Prettyprint.Doc (Pretty(pretty))
import Data.Text.Prettyprint.Doc.Render.Text (renderLazy)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Prettyprint.Doc.Internal (layoutPretty)
import Data.Text.Prettyprint.Doc (defaultLayoutOptions)
import System.FilePath (dropExtension, takeBaseName, (</>))

main :: IO ()
main =
  tests >>= defaultMain

prettyTest :: FilePath -> TestTree
prettyTest f =
  goldenVsStringDiff (dropExtension (takeBaseName f)) (\ref new -> ["diff", "-u", ref, new]) (f ++ ".golden.scala") $ do
    parsed <- eitherDecodeFileStrict f :: IO (Either String Source)
    either error (pure . encodeUtf8 . renderLazy . layoutPretty defaultLayoutOptions . pretty) parsed

tests :: IO TestTree
tests =
  testGroup "Golden" . map prettyTest <$> findByExtension [".json"] ("test" </> "fixtures")
