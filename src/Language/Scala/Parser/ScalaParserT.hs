{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Scala.Parser.ScalaParserT where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.IO.Class
import Language.Scala.Parser as P
import Language.Scala.Parser.ScalaTokenParsing
import Text.Trifecta.Delta
import qualified Data.HashSet as HashSet
import qualified Text.Trifecta as T

newtype ScalaParserT m a = ScalaParserT { unScalaParser :: m a }
    deriving (Functor, Applicative, Alternative)

deriving instance Monad m => Monad (ScalaParserT m)
deriving instance T.Parsing m => T.Parsing (ScalaParserT m)
deriving instance T.CharParsing m => T.CharParsing (ScalaParserT m)

instance (Monad m, T.CharParsing m) => ScalaTokenParsing (ScalaParserT m) where
    someSpace = () <$ some P.whiteSpace
    symbol name = token (T.string name)
    symbolic name = token (T.char name)
    token p = (someSpace <|> pure ()) *> p
    brackets = T.between (symbolic '[') (symbolic ']')
    parens = T.between (symbolic '(') (symbolic ')')
    commaSep p = T.sepBy p comma
    comma = symbolic ','
    ident s = do
      name <- (:) <$> T._styleStart s <*> many (T._styleLetter s) -- <?> T._styleName s
      when (HashSet.member name (T._styleReserved s)) $ T.unexpected $ "reserved " ++ T._styleName s ++ " " ++ show name
      return name

parseString :: ScalaParserT T.Parser a -> Delta -> String -> T.Result a
parseString (ScalaParserT p) = T.parseString p

parseFromFile :: MonadIO m => ScalaParserT T.Parser a -> String -> m (Maybe a)
parseFromFile (ScalaParserT p) = T.parseFromFile p
