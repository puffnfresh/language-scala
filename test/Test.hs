module Main where

import Data.Aeson (eitherDecodeFileStrict)
import Language.Scala (Source)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Data.Text.Prettyprint.Doc (Pretty(pretty))
import qualified Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main =
  tests >>= defaultMain

prettyTest :: FilePath -> TestTree
prettyTest f =
  goldenVsString f (f ++ ".golden.scala") $ do
    parsed <- eitherDecodeFileStrict f :: IO (Either String Source)
    pure (BS.pack (either error (show . pretty) parsed))

tests :: IO TestTree
tests =
  testGroup "Tests" . map prettyTest <$> findByExtension [".json"] "test"
