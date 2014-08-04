module Main where

import Language.Scala.Parser
import Language.Scala.Parser.ScalaParserT

import Data.Functor
import System.IO
import Test.Tasty
import Test.Tasty.Golden

main :: IO ()
main = tests >>= defaultMain

parseTest :: FilePath -> TestTree
parseTest f = goldenVsFile f expected actual action
    where
      expected = f ++ ".expected"
      actual = f ++ ".actual"
      action = do
        ast <- parseFromFile compilationUnit f
        writeFile actual (show ast)

tests :: IO TestTree
tests = testGroup "Tests" . map parseTest <$> findByExtension [".scala"] "test"
