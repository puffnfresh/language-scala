module Language.Scala.Parser.ScalaTokenParsing where

import Text.Parser.Token (IdentifierStyle)
import Data.Functor
import Text.Trifecta (CharParsing, optional)

class CharParsing m => ScalaTokenParsing m where
    someSpace :: m ()
    symbol :: String -> m String
    symbolic :: Char -> m Char
    token :: m a -> m a
    brackets :: m a -> m a
    parens :: m a -> m a
    commaSep :: m a -> m [a]
    comma :: m Char
    ident :: IdentifierStyle m -> m String

skipSpace :: ScalaTokenParsing m => m ()
skipSpace = () <$ optional someSpace
