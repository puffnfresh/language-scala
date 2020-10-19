{-# LANGUAGE OverloadedStrings #-}

{-
Calls out to the scalameta-parsers npm library using node. The library parses
Scala code and dumps out the AST in JSON.
-}

module Language.Scala.Parser.External (runNodeScalametaParsers, nodeParser) where

import Control.Applicative (Alternative ((<|>)))
import Data.Aeson
  ( FromJSON (..),
    eitherDecode,
    withObject,
    (.:),
  )
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Scala (Source)
import Paths_language_scala (getDataFileName)
import System.Process (readProcess)

data NodeResult
  = NodeError String Word Word
  | NodeSuccess Source
  deriving (Eq, Ord, Read, Show)

instance FromJSON NodeResult where
  parseJSON a =
    parseError <|> (NodeSuccess <$> parseJSON a)
    where
      parseError =
        withObject
          "NodeError"
          ( \o ->
              NodeError
                <$> o .: "error"
                <*> o .: "lineNumber"
                <*> o .: "columnNumber"
          )
          a

runNodeScalametaParsers :: String -> IO String
runNodeScalametaParsers s = do
  script <- getDataFileName "scalameta-parsers-to-json.js"
  readProcess "node" [script] s

nodeParser :: FromJSON a => String -> IO (Either String a)
nodeParser =
  (eitherDecode . encodeUtf8 . pack <$>) . runNodeScalametaParsers
