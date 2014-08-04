module Language.Scala.Parser where

import Control.Applicative
import Data.Maybe
import Language.Scala.Parser.AST
import Language.Scala.Parser.ScalaTokenParsing
import Text.Parser.Token (IdentifierStyle(..))
import Text.Trifecta (char, satisfyRange, oneOf, try, eof, manyTill, skipMany, sepBy1, sepEndBy)
import qualified Text.Parser.Token.Highlight as Highlight
import qualified Data.HashSet as HashSet

{-
  This is an attempt at using the Scala "syntax summary" which is
  sadly often wrong. Still would rather use it as a guide than not.

  http://www.scala-lang.org/files/archive/spec/2.11/13-syntax-summary.html
-}

reservedKeywords :: [String]
reservedKeywords =
    [ "abstract"
    , "case"
    , "catch"
    , "class"
    , "def"
    , "do"
    , "else"
    , "extends"
    , "false"
    , "final"
    , "finally"
    , "for"
    , "forSome"
    , "if"
    , "implicit"
    , "import"
    , "lazy"
    , "match"
    , "new"
    , "null"
    , "object"
    , "override"
    , "package"
    , "private"
    , "protected"
    , "return"
    , "sealed"
    , "super"
    , "this"
    , "trait"
    , "true"
    , "try"
    , "val"
    , "var"
    , "while"
    , "with"
    , "yield"
    ]

varStyle :: ScalaTokenParsing m => IdentifierStyle m
varStyle = IdentifierStyle { _styleName     = "identifier"
                           , _styleStart    = lower
                           , _styleLetter   = letter <|> digit
                           , _styleReserved = HashSet.fromList reservedKeywords
                           , _styleHighlight = Highlight.Identifier
                           , _styleReservedHighlight = Highlight.ReservedIdentifier
                           }

reservedOps :: [String]
reservedOps =
    [ "."
    , "_"
    , ":"
    , "="
    , "=>"
    , "<-"
    , "<:"
    , "<%"
    , ">:"
    , "#"
    , "@"
    ]

opStyle :: ScalaTokenParsing m => IdentifierStyle m
opStyle = IdentifierStyle { _styleName     = "operator"
                          , _styleStart    = opchar
                          , _styleLetter   = opchar
                          , _styleReserved = HashSet.fromList reservedOps
                          , _styleHighlight = Highlight.Operator
                          , _styleReservedHighlight = Highlight.ReservedOperator
                          }

-- Lexical syntax

nonSemiWhiteSpace :: ScalaTokenParsing m => m ()
nonSemiWhiteSpace = () <$ oneOf " \t\x000D"

whiteSpace :: ScalaTokenParsing m => m ()
whiteSpace = () <$ oneOf " \t\x000D\n"

lower :: ScalaTokenParsing m => m Char
lower = satisfyRange 'a' 'z'

upper :: ScalaTokenParsing m => m Char
upper = satisfyRange 'A' 'Z' <|> oneOf "$_"

letter :: ScalaTokenParsing m => m Char
letter = upper <|> lower

opchar :: ScalaTokenParsing m => m Char
opchar = oneOf "!#%&*+-/<=>?@\\^|~"

printableChar :: ScalaTokenParsing m => m Char
printableChar = satisfyRange '\x0020' '\x007F'

charEscapeSeq :: ScalaTokenParsing m => m Char
charEscapeSeq = char '\\' *> (('\b' <$ char 'b')
                          <|> ('\t' <$ char 't')
                          <|> ('\n' <$ char 'n')
                          <|> ('\f' <$ char 'f')
                          <|> ('\r' <$ char 'r')
                          <|> ('"' <$ char '"')
                          <|> ('\'' <$ char '\'')
                          <|> ('\\' <$ char '\\'))

op :: (Monad m, ScalaTokenParsing m) => m String
op = ident opStyle

--op :: ScalaTokenParsing m => m String
--op = some opchar

varid :: (Monad m, ScalaTokenParsing m) => m String
varid = token (ident varStyle)

 --varid :: ScalaTokenParsing m => m String
--varid = (:) <$> lower <*> idrest

plainid :: (Monad m, ScalaTokenParsing m) => m String
plainid = token ((:) <$> upper <*> idrest)
      <|> varid
      <|> op

id' :: (Monad m, ScalaTokenParsing m) => m String
id' = plainid

-- Very sure the spec is wrong on this.
idrest :: (Monad m, ScalaTokenParsing m) => m String
idrest = (++) <$> many (letter <|> digit) <*> (fromMaybe [] <$> optional ((:) <$> char '_' <*> op))

integerLiteral :: ScalaTokenParsing m => m Literal
integerLiteral = IntegerLiteral <$> some decimalNumeral

decimalNumeral :: ScalaTokenParsing m => m Char
decimalNumeral = digit

digit :: ScalaTokenParsing m => m Char
digit = char '0' <|> nonZeroDigit

nonZeroDigit :: ScalaTokenParsing m => m Char
nonZeroDigit = satisfyRange '1' '9'

floatingPointLiteral :: ScalaTokenParsing m => m Literal
floatingPointLiteral = empty

booleanLiteral :: ScalaTokenParsing m => m Literal
booleanLiteral = BooleanLiteral <$> (True <$ symbol "true" <|> False <$ symbol "false")

characterLiteral :: ScalaTokenParsing m => m Literal
characterLiteral = char '\'' *> (CharacterLiteral <$> (charEscapeSeq <|> printableChar)) <* char '\''

stringElement :: ScalaTokenParsing m => m Char
stringElement = charEscapeSeq <|> printableChar

stringLiteral :: ScalaTokenParsing m => m Literal
stringLiteral = char '"' *> (StringLiteral <$> manyTill stringElement (try (char '"')))

symbolLiteral :: (Monad m, ScalaTokenParsing m) => m Literal
symbolLiteral = SymbolLiteral <$> (char '\'' *> plainid)

nullLiteral :: ScalaTokenParsing m => m Literal
nullLiteral = NullLiteral <$ symbol "null"

semi :: ScalaTokenParsing m => m Char
semi = oneOf ";\n"

-- Context-free syntax

literal :: (Monad m, ScalaTokenParsing m) => m Literal
literal = integerLiteral
      <|> booleanLiteral
      <|> try characterLiteral
      <|> stringLiteral
      <|> symbolLiteral
      <|> nullLiteral

qualId :: (Monad m, ScalaTokenParsing m) => m [String]
qualId = sepBy1 (token id') (try (symbolic '.'))

path :: (Monad m, ScalaTokenParsing m) => m Expr
path = (\a b -> foldl SelectExpr a (fromMaybe [] b)) <$> (IdentExpr <$> id') <*> optional (symbolic '.' *> stableId)

stableId :: (Monad m, ScalaTokenParsing m) => m [String]
stableId = (\a b -> a:fromMaybe [] b) <$> id' <*> optional (try (symbolic '.') *> stableId)

type' :: (Monad m, ScalaTokenParsing m) => m Type
type' = infixType

infixType :: (Monad m, ScalaTokenParsing m) => m Type
infixType = compoundType -- <* optional (try (someSpace *> token id' <* compoundType))

compoundType :: (Monad m, ScalaTokenParsing m) => m Type
compoundType = annotType

annotType :: (Monad m, ScalaTokenParsing m) => m Type
annotType = simpleType

simpleType :: (Monad m, ScalaTokenParsing m) => m Type
simpleType = TypeRef <$> stableId <* optional typeArgs
         <|> TypeTuple <$> (char '(' *> types <* char ')')

typeArgs :: (Monad m, ScalaTokenParsing m) => m [Type]
typeArgs = char '[' *> types <* char ']'

types :: (Monad m, ScalaTokenParsing m) => m [Type]
types = commaSep (token type')

expr :: (Monad m, ScalaTokenParsing m) => m Expr
expr = expr1

expr1 :: (Monad m, ScalaTokenParsing m) => m Expr
expr1 = postfixExpr

postfixExpr :: (Monad m, ScalaTokenParsing m) => m Expr
postfixExpr = infixExpr

data InfixExpr = InfixSelectApply String Expr (Maybe InfixExpr)

infixExpr :: (Monad m, ScalaTokenParsing m) => m Expr
infixExpr = f prefixExpr
    where f a = h <$> a <*> b
          h = maybe <*> g
          g a (InfixSelectApply s e m) = h (ApplyExpr (SelectExpr a s) [e]) m
          b = optional (InfixSelectApply <$> try (someSpace *> id' <* someSpace) <*> prefixExpr <*> b)

prefixExpr :: (Monad m, ScalaTokenParsing m) => m Expr
prefixExpr = optional (oneOf "-+~!") *> simpleExpr

simpleExpr :: (Monad m, ScalaTokenParsing m) => m Expr
simpleExpr = NewExpr <$> (symbol "new" *> (classTemplate <|> templateBody))
         <|> blockExpr
         <|> simpleExpr1

data SimpleExpr1 = SimpleSelect1 String (Maybe SimpleExpr1)
                 | SimpleTypeApply1 [Type] (Maybe SimpleExpr1)
                 | SimpleApply1 [Expr] (Maybe SimpleExpr1)

-- TODO: There has got to be a much better way to do this.
simpleExpr1 :: (Monad m, ScalaTokenParsing m) => m Expr
simpleExpr1 = f $ LiteralExpr <$> literal
              <|> path
    where f a = h <$> a <*> b
          h = maybe <*> g
          g a (SimpleSelect1 s m) = h (SelectExpr a s) m
          g a (SimpleTypeApply1 t m) = h (TypeApplyExpr a t) m
          g a (SimpleApply1 e m) = h (ApplyExpr a e) m
          b = optional $ char '.' *> (SimpleSelect1 <$> id' <*> b)
                     <|> SimpleTypeApply1 <$> typeArgs <*> b
                     <|> SimpleApply1 <$> argumentExprs <*> b

exprs :: (Monad m, ScalaTokenParsing m) => m [Expr]
exprs = commaSep expr

argumentExprs :: (Monad m, ScalaTokenParsing m) => m [Expr]
argumentExprs = char '(' *> exprs <* char ')'

blockExpr :: (Monad m, ScalaTokenParsing m) => m Expr
blockExpr = BlockExpr <$> (char '{' *> block <* char '}')

block :: (Monad m, ScalaTokenParsing m) => m [BlockStat]
block = sepEndBy blockStat (some semi)

blockStat :: (Monad m, ScalaTokenParsing m) => m BlockStat
blockStat = BlockStatImport <$> import'
        <|> BlockStatDef <$> (optional (symbol "implicit") *> def)
        <|> BlockStatTmplDef <$> (many annotation *> many (localModifier <* someSpace) *> tmplDef)
        <|> BlockStatExpr <$> expr1

paramClauses :: (Monad m, ScalaTokenParsing m) => m [[Param]]
paramClauses = many paramClause <* optional (parens (symbol "implicit" *> params))

paramClause :: (Monad m, ScalaTokenParsing m) => m [Param]
paramClause = parens params

params :: (Monad m, ScalaTokenParsing m) => m [Param]
params = commaSep param

param :: (Monad m, ScalaTokenParsing m) => m Param
param = Param <$> id' <* symbolic ':' <*> paramType

paramType :: (Monad m, ScalaTokenParsing m) => m Type
paramType = type'

classParamClauses :: (Monad m, ScalaTokenParsing m) => m [[Param]]
classParamClauses = many classParamClause

classParamClause :: (Monad m, ScalaTokenParsing m) => m [Param]
classParamClause = char '(' *> skipMany whiteSpace *> (commaSep classParam) <* char ')'

classParam :: (Monad m, ScalaTokenParsing m) => m Param
classParam = Param <$> (many annotation *> many (modifier <* someSpace) *> optional (symbol "val" <|> symbol "var") *> token id') <* symbolic ':' <*> token paramType <* optional (symbolic '=' <* expr)

modifier :: ScalaTokenParsing m => m Modifier
modifier = localModifier

localModifier :: ScalaTokenParsing m => m Modifier
localModifier = Abstract <$ symbol "abstract"
            <|> Final <$ symbol "final"
            <|> Sealed <$ symbol "sealed"
            <|> Implicit <$ symbol "implicit"
            <|> Lazy <$ symbol "lazy"

accessModifier :: (Monad m, ScalaTokenParsing m) => m ()
accessModifier = () <$ (symbol "private" <|> symbol "protected") <* optional accessQualifier

accessQualifier :: (Monad m, ScalaTokenParsing m) => m ()
accessQualifier = () <$ (char '[' *> id' <* char ']')

annotation :: (Monad m, ScalaTokenParsing m) => m ()
annotation = () <$ symbolic '@' <* token simpleType

templateBody :: (Monad m, ScalaTokenParsing m) => m TemplateBody
templateBody = TemplateBody <$> (symbolic '{' *> (sepEndBy templateStat semi <* skipMany whiteSpace) <* char '}')

templateStat :: (Monad m, ScalaTokenParsing m) => m TemplateStat
templateStat = someSpace *> (TemplateStatImport <$> import'
                        <|> try (TemplateStatDef <$> (many annotation *> many (modifier <* someSpace)) <*> def)
                        <|> TemplateStatDcl <$> dcl
                        <|> TemplateStatExpr <$> expr)

import' :: (Monad m, ScalaTokenParsing m) => m [[String]]
import' = symbol "import" *> someSpace *> sepBy1 (token importExpr) (try comma)

importExpr :: (Monad m, ScalaTokenParsing m) => m [String]
importExpr = stableId

importSelectors :: ScalaTokenParsing m => m [String]
importSelectors = empty

dcl :: (Monad m, ScalaTokenParsing m) => m DclDef
dcl = symbol "val" *> valDcl
  <|> symbol "def" *> funDcl
  <|> symbol "type" *> typeDcl

valDcl :: (Monad m, ScalaTokenParsing m) => m DclDef
valDcl = DclVal <$> token id' <* char ':' <*> type'

funDcl :: (Monad m, ScalaTokenParsing m) => m DclDef
funDcl = (\(a, b, c) -> DclDef a b c) <$> funSig <* optional (char ':' <* type')

funSig :: (Monad m, ScalaTokenParsing m) => m (String, [String], [[Param]])
funSig = (,,) <$> token id' <*> (skipSpace *> (fromMaybe [] <$> optional funTypeParamClause)) <*> paramClauses

typeDcl :: (Monad m, ScalaTokenParsing m) => m DclDef
typeDcl = DclType <$> token id' <* optional typeParamClause <* optional (symbol "<:" <* type')

patVarDef :: (Monad m, ScalaTokenParsing m) => m DefDef
patVarDef = symbol "val" *> patDef

def :: (Monad m, ScalaTokenParsing m) => m DefDef
def = patVarDef
  <|> symbol "def" *> someSpace *> funDef
  <|> symbol "type" *> someSpace *> typeDef

patDef :: (Monad m, ScalaTokenParsing m) => m DefDef
patDef = CaseDef <$> token pattern2 <* skipSpace <* optional (char ':' *> token type') <* char '=' <*> (skipSpace *> expr)

pattern :: (Monad m, ScalaTokenParsing m) => m Match
pattern = pattern1

pattern1 :: (Monad m, ScalaTokenParsing m) => m Match
pattern1 = pattern2

pattern2 :: (Monad m, ScalaTokenParsing m) => m Match
pattern2 = MatchId . (:[]) <$> varid
       <|> pattern3

pattern3 :: (Monad m, ScalaTokenParsing m) => m Match
pattern3 = simplePattern

simplePattern :: (Monad m, ScalaTokenParsing m) => m Match
simplePattern = parens patterns
            <|> MatchId <$> stableId

patterns :: (Monad m, ScalaTokenParsing m) => m Match
patterns = MatchTuple <$> commaSep pattern

typeParamClause :: (Monad m, ScalaTokenParsing m) => m [String]
typeParamClause = brackets (commaSep variantTypeParam)

funTypeParamClause :: (Monad m, ScalaTokenParsing m) => m [String]
funTypeParamClause = brackets (commaSep typeParam)

variantTypeParam :: (Monad m, ScalaTokenParsing m) => m String
variantTypeParam = optional (oneOf "+-") *> typeParam

typeParam :: (Monad m, ScalaTokenParsing m) => m String
typeParam = token id' <* optional typeParamClause <* optional (symbol ">:" <* token type') <* optional (symbol "<:" <* token type') <* many (char ':' <* type')

funDef :: (Monad m, ScalaTokenParsing m) => m DefDef
funDef = (\(a, b, c) -> DefDef a b c) <$> funSig <* skipSpace <* optional (char ':' *> token type') <* char '=' <*> (skipSpace *> expr)

typeDef :: (Monad m, ScalaTokenParsing m) => m DefDef
typeDef = TypeDef <$> token id' <* skipSpace <*> (fromMaybe [] <$> optional typeParamClause) <* char '=' <*> type'

tmplDef :: (Monad m, ScalaTokenParsing m) => m TmplDef
tmplDef = optional (symbol "case") *> (symbol "object" *> objectDef <|> symbol "class" *> classDef) <|> symbol "trait" *> someSpace *> traitDef

classDef :: (Monad m, ScalaTokenParsing m) => m TmplDef
classDef = ClassDef <$> id' <* optional someSpace <*> (fromMaybe [] <$> optional typeParamClause) <* optional accessModifier <*> classParamClauses <*> classTemplateOpt

traitDef :: (Monad m, ScalaTokenParsing m) => m TmplDef
traitDef = TraitDef <$> id' <* optional someSpace <*> (fromMaybe [] <$> optional typeParamClause) <*> traitTemplateOpt

objectDef :: (Monad m, ScalaTokenParsing m) => m TmplDef
objectDef = ObjectDef <$> id' <*> classTemplateOpt

classTemplateOpt :: (Monad m, ScalaTokenParsing m) => m TemplateBody
classTemplateOpt = fromMaybe (TemplateBody []) <$> optional (try (someSpace *> (symbol "extends" *> someSpace *> classTemplate <|> templateBody)))

traitTemplateOpt :: (Monad m, ScalaTokenParsing m) => m TemplateBody
traitTemplateOpt = fromMaybe (TemplateBody []) <$> optional traitTemplate

classTemplate :: (Monad m, ScalaTokenParsing m) => m TemplateBody
classTemplate = classParents *> (fromMaybe (TemplateBody []) <$> optional (try (optional someSpace *> templateBody)))

traitTemplate :: (Monad m, ScalaTokenParsing m) => m TemplateBody
traitTemplate = templateBody

classParents :: (Monad m, ScalaTokenParsing m) => m Type
classParents = constr <* many (try (symbol "with") *> annotType)

constr :: (Monad m, ScalaTokenParsing m) => m Type
constr = annotType <* many argumentExprs

packaging :: (Monad m, ScalaTokenParsing m) => m TopStat
packaging = Packaging <$> (symbol "package" *> token qualId)

packageObject :: (Monad m, ScalaTokenParsing m) => m TopStat
packageObject = PackageObject <$> (try (symbol "package" *> symbol "object") *> objectDef)

topStat :: (Monad m, ScalaTokenParsing m) => m TopStat
topStat = TmplDef <$> (many (modifier <* someSpace) *> tmplDef)
      <|> Import <$> import'
      <|> packageObject
      <|> packaging

compilationUnit :: (Monad m, ScalaTokenParsing m) => m [TopStat]
compilationUnit = sepEndBy topStat (some semi) <* eof
