{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module: Language.Scala
--
-- Syntax trees representing Scala.
--
-- Each 'Pretty' instance should generate valid Scala code.
--
-- Each 'FromJSON' instance should parse JSON produced from
-- <https://www.npmjs.com/package/scalameta-parsers scalameta-parsers> (also
-- available via <https://astexplorer.net/ AST Explorer>)
module Language.Scala
  ( Source (..),
    Stat (..),
    Decl (..),
    Defn (..),
    CtorPrimary (..),
    Pat (..),
    Lit (..),
    Type (..),
    TypeParam (..),
    Term (..),
    Enumerator (..),
    Case (..),
    TermParam (..),
    TermRef (..),
    Self (..),
    Bounds (..),
    TypeRef (..),
    Ref (..),
    Importer (..),
    Importee (..),
    Init (..),
    Template (..),
    Mod (..),
  )
where

import Control.Applicative (Alternative (empty))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Aeson
  ( FromJSON (parseJSON),
    Object,
    Value (Object),
    withObject,
    (.:),
    (.:?),
  )
import Data.Aeson.Types
  ( Parser,
    explicitParseField,
    listParser,
    typeMismatch,
  )
import Data.Char (ord, chr, isAlphaNum)
import Data.Foldable (asum)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ord (comparing)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
  ( Doc,
    Pretty (..),
    brackets,
    comma,
    concatWith,
    encloseSep,
    hardline,
    hsep,
    indent,
    lbrace,
    list,
    lparen,
    parens,
    punctuate,
    rbrace,
    rparen,
    semi,
    space,
    surround,
    tupled,
    (<+>),
  )
import Data.Word (Word16)

data Source
  = Source [Stat]
  deriving (Eq, Ord, Read, Show)

instance Pretty Source where
  pretty (Source stats) =
    hardlines (pretty <$> stats)

instance FromJSON Source where
  parseJSON =
    withObject
      "Source"
      ( \o ->
          Source <$> (o .: "stats")
      )

data Stat
  = StatTerm Term
  | StatDecl Decl
  | StatDefn Defn
  | StatImport [Importer]
  | StatPkg TermRef [Stat]
  | StatPkgObject [Mod] Text Template
  | StatCtorSecondary [Mod] [[TermParam]] Init [Stat]
  deriving (Eq, Ord, Read, Show)

instance Pretty Stat where
  pretty (StatTerm term) =
    pretty term
  pretty (StatDecl decl) =
    pretty decl
  pretty (StatDefn defn) =
    pretty defn
  pretty (StatImport importers) =
    "import" <+> hsep (punctuate comma (pretty <$> importers))
  pretty (StatPkg ref stats) =
    "package" <+> pretty ref <+> hardlines ([lbrace] <> (indent 2 . pretty <$> stats) <> [rbrace])
  pretty (StatPkgObject mods name templ) =
    "package" <+> hsep (pretty <$> mods) <+> "object" <+> pretty name <+> pretty templ
  pretty (StatCtorSecondary mods paramss init' stats) =
    hsep (pretty <$> mods) <+> "def this" <> paramss' <+> "=" <+> stats'
    where
      paramss' =
        foldMap (\p -> tupled (pretty <$> p)) paramss
      stats' =
        if null stats
          then pretty init'
          else hardlines ([lbrace, pretty init'] <> (pretty <$> stats) <> [rbrace])

parseStat :: Text -> Object -> MaybeT Parser Stat
parseStat t o =
  case t of
    "Import" ->
      lift
        ( StatImport
            <$> o .: "importers"
        )
    "Pkg" ->
      lift
        ( StatPkg
            <$> o .: "ref"
            <*> o .: "stats"
        )
    "Pkg.Object" ->
      lift
        ( StatPkgObject
            <$> o .: "mods"
            <*> explicitParseField nameParser o "name"
            <*> o .: "templ"
        )
    "Ctor.Secondary" ->
      lift
        ( StatCtorSecondary
            <$> o .: "mods"
            <*> o .: "paramss"
            <*> o .: "init"
            <*> o .: "stats"
        )
    _ ->
      asum
        [ StatDefn
            <$> parseDefn t o,
          StatDecl
            <$> parseDecl t o,
          StatTerm
            <$> parseTerm t o
        ]

instance FromJSON Stat where
  parseJSON =
    withTypeObject "Stat" parseStat

data Decl
  = DeclVal [Mod] [Pat] Type
  | -- | DeclVar [Mod] [Pat] Type
    DeclDef [Mod] Text [TypeParam] [[TermParam]] Type
  --- | DeclType [Mod] Text [TypeParam] Bounds
  deriving (Eq, Ord, Read, Show)

instance Pretty Decl where
  pretty (DeclVal mods pats decltpe) =
    hsep ((pretty <$> mods) ++ ["val", concatWith (surround (comma <> space)) (pretty <$> pats) <> ":", pretty decltpe])
  pretty (DeclDef mods name tparams paramss decltpe) =
    hsep ((pretty <$> mods) ++ ["def", pretty name <> tparams' <> paramss' <> decltpe'])
    where
      decltpe' =
        prettyDecltpeWithName tparams paramss name (Just decltpe)
      tparams' =
        if null tparams
          then mempty
          else list (pretty <$> tparams)
      paramss' =
        foldMap (\p -> tupled (pretty <$> p)) paramss

parseDecl :: Text -> Object -> MaybeT Parser Decl
parseDecl t o =
  case t of
    "Decl.Def" ->
      lift
        ( DeclDef
            <$> o .: "mods"
            <*> explicitParseField nameParser o "name"
            <*> o .: "tparams"
            <*> o .: "paramss"
            <*> o .: "decltpe"
        )
    "Decl.Val" ->
      lift
        ( DeclVal
            <$> o .: "mods"
            <*> o .: "pats"
            <*> o .: "decltpe"
        )
    _ ->
      empty

instance FromJSON Decl where
  parseJSON =
    withTypeObject "Decl" parseDecl

data Defn
  = DefnVal [Mod] [Pat] (Maybe Type) Term
  | --- | DefnVar [Mod] [Pat] (Maybe Type) (Maybe Term)
    DefnDef [Mod] Text [TypeParam] [[TermParam]] (Maybe Type) Term
  | --- | DefnMacro [Mod] Text [TypeParam] [[TermParam]] (Maybe Type) Term
    DefnType [Mod] Text [TypeParam] Type
  | DefnClass [Mod] Text [TypeParam] CtorPrimary Template
  | DefnTrait [Mod] Text [TypeParam] CtorPrimary Template
  | DefnObject [Mod] Text Template
  deriving (Eq, Ord, Read, Show)

instance Pretty Defn where
  pretty (DefnVal mods pats decltpe body) =
    hardlines
      [ hsep ((pretty <$> mods) ++ ["val", concatWith (surround (comma <> space)) (pretty <$> pats) <> prettyDecltpe decltpe, "="]),
        indent 2 (pretty body)
      ]
  pretty (DefnDef mods name tparams paramss decltpe body) =
    hardlines
      [ hsep ((pretty <$> mods) ++ ["def", pretty name <> tparams' <> paramss' <> decltpe', "="]),
        indent 2 (pretty body)
      ]
    where
      decltpe' =
        prettyDecltpeWithName tparams paramss name decltpe
      tparams' =
        if null tparams
          then mempty
          else list (pretty <$> tparams)
      paramss' =
        foldMap (\p -> tupled (pretty <$> dropImplicits p)) paramss
      dropImplicits (p : ps) =
        p : (removeImplicit <$> ps)
      dropImplicits [] =
        []
      removeImplicit (TermParam mods' name' decltpe'' default') =
        TermParam (filter (/= ModImplicit) mods') name' decltpe'' default'
  pretty (DefnObject mods name templ) =
    hsep ((pretty <$> mods) ++ ["object", pretty name, prettyTemplate templ])
  pretty (DefnTrait mods name tparams ctor templ) =
    hsep ((pretty <$> mods) ++ ["trait", pretty name <> tparams' <> pretty ctor, prettyTemplate templ])
    where
      tparams' =
        if null tparams
          then mempty
          else list (pretty <$> tparams)
  pretty (DefnType mods name tparams body) =
    hsep ((pretty <$> mods) ++ ["type", pretty name <> tparams', "=", pretty body])
    where
      tparams' =
        if null tparams
          then mempty
          else list (pretty <$> tparams)
  pretty (DefnClass mods name tparams ctor templ) =
    hsep ((pretty <$> mods) ++ ["class", pretty name <> tparams' <> pretty ctor, prettyTemplate templ])
    where
      tparams' =
        if null tparams
          then mempty
          else list (pretty <$> tparams)

parseDefn :: Text -> Object -> MaybeT Parser Defn
parseDefn t o =
  case t of
    "Defn.Val" ->
      lift
        ( DefnVal
            <$> o .: "mods"
            <*> o .: "pats"
            <*> o .:? "decltpe"
            <*> o .: "rhs"
        )
    "Defn.Def" ->
      lift
        ( DefnDef
            <$> o .: "mods"
            <*> explicitParseField nameParser o "name"
            <*> o .: "tparams"
            <*> o .: "paramss"
            <*> o .:? "decltpe"
            <*> o .: "body"
        )
    "Defn.Type" ->
      lift
        ( DefnType
            <$> o .: "mods"
            <*> explicitParseField nameParser o "name"
            <*> o .: "tparams"
            <*> o .: "body"
        )
    "Defn.Class" ->
      lift
        ( DefnClass
            <$> o .: "mods"
            <*> explicitParseField nameParser o "name"
            <*> o .: "tparams"
            <*> o .: "ctor"
            <*> o .: "templ"
        )
    "Defn.Trait" ->
      lift
        ( DefnTrait
            <$> o .: "mods"
            <*> explicitParseField nameParser o "name"
            <*> o .: "tparams"
            <*> o .: "ctor"
            <*> o .: "templ"
        )
    "Defn.Object" ->
      lift
        ( DefnObject
            <$> o .: "mods"
              <*> explicitParseField nameParser o "name"
              <*> o .: "templ"
        )
    _ ->
      empty

instance FromJSON Defn where
  parseJSON =
    withTypeObject "Defn" parseDefn

data CtorPrimary
  = CtorPrimary [Mod] [[TermParam]]
  deriving (Eq, Ord, Read, Show)

instance Pretty CtorPrimary where
  pretty (CtorPrimary mods paramss) =
    hsep (pretty <$> mods) <+> foldMap (\p -> tupled (pretty <$> p)) paramss

instance FromJSON CtorPrimary where
  parseJSON =
    withObject
      "Ctor.Primary"
      ( \o ->
          CtorPrimary
            <$> o .: "mods"
            <*> o .: "paramss"
      )

data Pat
  = PatLit Lit
  | PatTermName Text
  | PatTermSelect Term Text
  | PatVar Text
  | PatWildcard
  | --- | PatSeqWildcard
    PatBind Pat Pat
  | PatAlternative Pat Pat
  | PatTuple [Pat]
  | PatExtract Term [Pat]
  | PatExtractInfix Pat Text [Pat]
  | --- | PatInterpolate Text [Lit] [Pat]
    --- | PatXml [Lit] [Pat]
    PatTyped Pat Type
  deriving (Eq, Ord, Read, Show)

prettyPat :: Pat -> ScalaDoc PatGroup
prettyPat (PatLit lit) =
  ScalaDoc PatGroupLiteral (pretty lit)
prettyPat (PatTermName name) =
  ScalaDoc PatGroupPath (pretty name)
prettyPat (PatTermSelect qual name) =
  ScalaDoc PatGroupPath (pretty qual <> "." <> pretty name)
prettyPat (PatVar name) =
  ScalaDoc PatGroupSimplePattern (pretty name)
prettyPat PatWildcard =
  ScalaDoc PatGroupSimplePattern "_"
prettyPat (PatBind lhs rhs) =
  ScalaDoc
    PatGroupPattern2
    ( hsep
        [ parensLeft PatGroupSimplePattern (prettyPat lhs),
          "@",
          parensLeft PatGroupAnyPattern3 (prettyPat rhs)
        ]
    )
prettyPat (PatAlternative lhs rhs) =
  ScalaDoc
    PatGroupPattern
    ( hsep
        [ parensLeft PatGroupPattern (prettyPat lhs),
          "|",
          parensLeft PatGroupPattern (prettyPat rhs)
        ]
    )
prettyPat (PatTuple args) =
  ScalaDoc PatGroupSimplePattern (tupled (pretty <$> args))
prettyPat (PatExtract fun args) =
  ScalaDoc PatGroupSimplePattern (pretty fun <+> tupled (pretty <$> args))
prettyPat (PatExtractInfix lhs op rhs) =
  ScalaDoc (PatGroupPattern3 op) (pretty lhs <+> pretty op <+> tupled (pretty <$> rhs))
prettyPat (PatTyped lhs rhs) =
  -- parensLeft TypeGroupRefineTyp (prettyType rhs)
  ScalaDoc PatGroupPattern1 (parensLeft PatGroupSimplePattern (prettyPat lhs) <> ":" <+> pretty rhs)

instance Pretty Pat where
  pretty =
    pretty . prettyPat

parsePat :: Text -> Object -> MaybeT Parser Pat
parsePat t o =
  case t of
    "Term.Name" ->
      lift
        ( PatTermName
            <$> nameParser (Object o)
        )
    "Term.Select" ->
      lift
        ( PatTermSelect
            <$> o .: "qual"
            <*> explicitParseField nameParser o "name"
        )
    "Pat.Var" ->
      lift
        ( PatVar
            <$> explicitParseField nameParser o "name"
        )
    "Pat.Wildcard" ->
      pure PatWildcard
    "Pat.Bind" ->
      lift
        ( PatBind
            <$> o .: "lhs"
            <*> o .: "rhs"
        )
    "Pat.Alternative" ->
      lift
        ( PatAlternative
            <$> o .: "lhs"
            <*> o .: "rhs"
        )
    "Pat.Tuple" ->
      lift
        ( PatTuple
            <$> o .: "args"
        )
    "Pat.Extract" ->
      lift
        ( PatExtract
            <$> o .: "fun"
            <*> o .: "args"
        )
    "Pat.ExtractInfix" ->
      lift
        ( PatExtractInfix
            <$> o .: "lhs"
            <*> explicitParseField nameParser o "op"
            <*> o .: "rhs"
        )
    "Pat.Typed" ->
      lift
        ( PatTyped
            <$> o .: "lhs"
            <*> o .: "rhs"
        )
    _ ->
      PatLit
        <$> parseLit t o

instance FromJSON Pat where
  parseJSON =
    withTypeObject "Pat" parsePat

data Lit
  = LitNull
  | LitInt Int32
  | LitDouble Scientific
  | LitFloat Scientific
  | LitByte Int8
  | LitShort Int16
  | LitChar Word16
  | LitLong Int64
  | LitBoolean Bool
  | LitUnit
  | LitString Text
  --- | LitSymbol Text
  deriving (Eq, Ord, Read, Show)

instance Pretty Lit where
  pretty LitNull =
    "null"
  pretty (LitInt value) =
    pretty value
  pretty (LitDouble value) =
    pretty (show value)
  pretty (LitFloat value) =
    pretty (show value) <> "f"
  pretty (LitByte value) =
    pretty value
  pretty (LitShort value) =
    pretty value
  pretty (LitChar value) =
    pretty (show (chr (fromIntegral value)))
  pretty (LitLong value) =
    pretty value <> "L"
  pretty (LitBoolean value) =
    if value
      then "true"
      else "false"
  pretty LitUnit =
    "()"
  pretty (LitString syntax) =
    "\"" <> pretty syntax <> "\""

parseLit :: Text -> Object -> MaybeT Parser Lit
parseLit t o =
  case t of
    "Lit.Null" ->
      pure LitNull
    "Lit.Int" ->
      lift
        ( LitInt
            <$> o .: "value"
        )
    "Lit.Double" ->
      lift
        ( LitDouble
            <$> o .: "value"
        )
    "Lit.Float" ->
      lift
        ( LitFloat
            <$> o .: "value"
        )
    "Lit.Byte" ->
      lift
        ( LitByte
            <$> o .: "value"
        )
    "Lit.Short" ->
      lift
        ( LitShort
            <$> o .: "value"
        )
    "Lit.Char" ->
      lift
        ( LitChar . fromIntegral . ord
            <$> o .: "value"
        )
    "Lit.Long" ->
      lift
        ( LitLong
            <$> o .: "value"
        )
    "Lit.Boolean" ->
      lift
        ( LitBoolean
            <$> o .: "value"
        )
    "Lit.Unit" ->
      pure LitUnit
    "Lit.String" ->
      lift
        ( LitString
            <$> nameParser (Object o)
        )
    _ ->
      empty

instance FromJSON Lit where
  parseJSON =
    withTypeObject "Lit" parseLit

data Type
  = TypeRef TypeRef
  | TypeApply Type [Type]
  | TypeApplyInfix Type Text Type
  | TypeFunction [Type] Type
  | TypeTuple [Type]
  | TypeWith Type Type
  | TypeRefine (Maybe Type) [Stat]
  | --- | TypeExistential Type [Stat]
    TypeAnnotate Type [Init]
  | TypePlaceholder Bounds
  | TypeByName Type
  | TypeRepeated Type
  --- | TypeVar Text
  deriving (Eq, Ord, Read, Show)

prettyType :: Type -> ScalaDoc TypeGroup
prettyType (TypeRef typeRef) =
  prettyTypeRef typeRef
prettyType (TypeApply tpe args) =
  ScalaDoc
    TypeGroupSimpleType
    (pretty tpe <> prettyList args)
prettyType (TypeApplyInfix lhs op rhs) =
  ScalaDoc
    (TypeGroupInfixType op)
    (parensLeft (TypeGroupInfixType op) (prettyType lhs) <+> pretty op <+> parensRight (TypeGroupInfixType op) (prettyType rhs))
prettyType (TypeFunction params res) =
  ScalaDoc
    TypeGroupType
    (params' <+> "=>" <+> parensLeft TypeGroupType (prettyType res))
  where
    params' =
      case params of
        [param]
          | not (isTuple param) ->
            parensLeft TypeGroupAnyInfixType (prettyType param)
        _ ->
          tupled (parensLeft TypeGroupParamType . prettyType <$> params)
    isTuple (TypeTuple _) =
      True
    isTuple _ =
      False
prettyType (TypeTuple args) =
  ScalaDoc
    TypeGroupSimpleType
    (tupled (pretty <$> args))
prettyType (TypeWith lhs rhs) =
  ScalaDoc
    TypeGroupWithType
    (pretty lhs <+> "with" <+> pretty rhs)
prettyType (TypeRefine tpe stats) =
  ScalaDoc
    TypeGroupRefineType
    (pretty tpe <+> encloseSep lbrace rbrace semi (pretty <$> stats))
prettyType (TypeAnnotate tpe annots) =
  ScalaDoc
    TypeGroupAnnotType
    (pretty tpe <+> hsep (("@" <>) . pretty <$> annots))
prettyType (TypePlaceholder bounds) =
  ScalaDoc
    TypeGroupSimpleType
    ("_" <> pretty bounds)
prettyType (TypeByName tpe) =
  ScalaDoc
    TypeGroupParamType
    ("=>" <+> pretty tpe)
prettyType (TypeRepeated tpe) =
  ScalaDoc
    TypeGroupParamType
    (pretty tpe <> "*")

instance Pretty Type where
  pretty =
    pretty . prettyType

parseType :: Text -> Object -> MaybeT Parser Type
parseType t o =
  case t of
    "Type.Apply" ->
      lift
        ( TypeApply
            <$> o .: "tpe"
            <*> o .: "args"
        )
    "Type.ApplyInfix" ->
      lift
        ( TypeApplyInfix
            <$> o .: "lhs"
            <*> explicitParseField nameParser o "op"
            <*> o .: "rhs"
        )
    "Type.Function" ->
      lift
        ( TypeFunction
            <$> o .: "params"
            <*> o .: "res"
        )
    "Type.Tuple" ->
      lift
        ( TypeTuple
            <$> o .: "args"
        )
    "Type.With" ->
      lift
        ( TypeWith
            <$> o .: "lhs"
            <*> o .: "rhs"
        )
    "Type.Refine" ->
      lift
        ( TypeRefine
            <$> o .:? "tpe"
            <*> o .: "stats"
        )
    "Type.Annotate" ->
      lift
        ( TypeAnnotate
            <$> o .: "tpe"
            <*> (o .: "annots" >>= listParser (withObject "Mod.Annot" (.: "init")))
        )
    "Type.Placeholder" ->
      lift
        ( TypePlaceholder
            <$> o .: "bounds"
        )
    "Type.ByName" ->
      lift
        ( TypeByName
            <$> o .: "tpe"
        )
    "Type.Repeated" ->
      lift
        ( TypeRepeated
            <$> o .: "tpe"
        )
    _ ->
      TypeRef
        <$> parseTypeRef t o

instance FromJSON Type where
  parseJSON =
    withTypeObject "Type" parseType

data TypeParam
  = TypeParam [Mod] Text [TypeParam] Bounds [Type] [Type]
  deriving (Eq, Ord, Read, Show)

instance Pretty TypeParam where
  pretty (TypeParam mods name tparams tbounds _vbounds cbounds) =
    mods' <> name' <> tparams' <> pretty tbounds <> cbounds'
    where
      mods' =
        if null mods
          then mempty
          else hsep (pretty <$> mods) <> space
      name' =
        if T.null name
          then "_"
          else pretty name
      tparams' =
        if null tparams
          then mempty
          else list (pretty <$> tparams)
      cbounds' =
        if null cbounds
          then mempty
          else ":" <+> concatWith (surround ": ") (pretty <$> cbounds)

instance FromJSON TypeParam where
  parseJSON =
    withObject
      "Type.Param"
      ( \o ->
          TypeParam <$> o .: "mods" <*> explicitParseField nameParser o "name" <*> o .: "tparams" <*> o .: "tbounds" <*> o .: "vbounds" <*> o .: "cbounds"
      )

data Term
  = TermLit Lit
  | TermRef TermRef
  | TermInterpolate Text [Lit] [Term]
  | --- | TermXml [Lit] [Term]
    TermApply Term [Term]
  | TermApplyType Term [Type]
  | TermApplyInfix Term Text [Type] [Term]
  | TermApplyUnary Text Term
  | TermAssign Term Term
  | --- | TermReturn Term
    TermThrow Term
  | TermAscribe Term Type
  | TermAnnotate Term [Init]
  | TermTuple [Term]
  | TermBlock [Stat]
  | TermIf Term Term Term
  | TermMatch Term [Case]
  | TermTry Term [Case] (Maybe Term)
  | --- | TermTryWithHandler Term Term (Maybe Term)
    TermFunction [TermParam] Term
  | TermPartialFunction [Case]
  | --- | TermWhile Term Term
    --- | TermDo Term Term
    --- | TermFor [Enumerator] Term
    TermForYield [Enumerator] Term
  | TermNew Init
  | TermNewAnonymous Template
  | TermPlaceholder
  | TermEta Term
  | TermRepeated Term
  deriving (Eq, Ord, Read, Show)

prettyTerm :: Term -> ScalaDoc TermGroup
prettyTerm (TermRef termRef) =
  prettyTermRef termRef
prettyTerm (TermInterpolate prefix parts args) =
  ScalaDoc
    TermGroupSimpleExpr1
    (pretty prefix <> "\"" <> go parts args <> "\"")
  where
    go (LitString p : ps) (a : as) =
      pretty p <> "${" <> pretty a <> "}" <> go ps as
    go (LitString p : _) _ =
      pretty p
    go _ _ =
      mempty
prettyTerm (TermApplyType fun targs) =
  ScalaDoc
    TermGroupSimpleExpr1
    (parensLeft TermGroupSimpleExpr (prettyTerm fun) <> targs')
  where
    targs' =
      if null targs
        then mempty
        else list (pretty <$> targs)
prettyTerm (TermApplyInfix lhs op targs args) =
  ScalaDoc
    (TermGroupInfixExpr op)
    (parensLeft (TermGroupInfixExpr op) (prettyTerm lhs) <+> pretty op <> targs' <+> args')
  where
    targs' =
      if null targs
        then mempty
        else list (pretty <$> targs)
    lhsIsPlaceHolder TermPlaceholder =
      True
    lhsIsPlaceHolder (TermRef (TermRefSelect lhs' _)) =
      lhsIsPlaceHolder lhs'
    lhsIsPlaceHolder (TermApply lhs' _) =
      lhsIsPlaceHolder lhs'
    lhsIsPlaceHolder (TermApplyInfix lhs' _ _ _) =
      lhsIsPlaceHolder lhs'
    lhsIsPlaceHolder _ =
      False
    args' =
      case args of
        [arg] ->
          if lhsIsPlaceHolder arg
            then parens (pretty arg)
            else parensRight (TermGroupInfixExpr op) (prettyTerm arg)
        _ ->
          tupled (pretty <$> args)
prettyTerm (TermApply fun args) =
  ScalaDoc
    TermGroupSimpleExpr1
    (parensLeft TermGroupSimpleExpr1 (prettyTerm fun) <> tupled (pretty <$> args))
prettyTerm (TermApplyUnary op arg) =
  ScalaDoc
    TermGroupPrefixExpr
    (pretty op <> parensLeft TermGroupSimpleExpr (prettyTerm arg))
prettyTerm (TermAssign lhs rhs) =
  ScalaDoc
    TermGroupExpr1
    (parensLeft TermGroupSimpleExpr1 (prettyTerm lhs) <+> "=" <+> parensLeft TermGroupExpr (prettyTerm rhs))
prettyTerm (TermLit lit) =
  ScalaDoc
    TermGroupLiteral
    (pretty lit)
prettyTerm (TermThrow expr) =
  ScalaDoc
    TermGroupExpr1
    ("throw" <+> parensLeft TermGroupExpr (prettyTerm expr))
prettyTerm (TermAscribe expr tpe) =
  ScalaDoc
    TermGroupExpr1
    (parensLeft TermGroupPostfixExpr (prettyTerm expr) <+> ":" <+> pretty tpe)
prettyTerm (TermAnnotate expr annots) =
  ScalaDoc
    TermGroupExpr1
    (parensLeft TermGroupPostfixExpr (prettyTerm expr) <> ":" <+> hsep (("@" <>) . pretty <$> annots))
prettyTerm (TermTuple args) =
  ScalaDoc
    TermGroupSimpleExpr1
    (tupled (pretty <$> args))
prettyTerm (TermBlock stats) =
  ScalaDoc
    TermGroupSimpleExpr
    (hardlines ([lbrace] <> (indent 2 . pretty <$> stats) <> [rbrace]))
prettyTerm (TermIf cond thenp elsep) =
  ScalaDoc
    TermGroupExpr1
    ( hardlines
        [ "if" <+> parens (pretty cond),
          indent 2 (parensLeft TermGroupExpr (prettyTerm thenp)),
          "else",
          indent 2 (parensLeft TermGroupExpr (prettyTerm elsep))
        ]
    )
prettyTerm (TermMatch expr cases) =
  ScalaDoc
    TermGroupExpr1
    (parensLeft TermGroupPostfixExpr (prettyTerm expr) <+> "match" <+> lbrace <> hardline <> hardlines (indent 2 . pretty <$> cases) <> hardline <> rbrace)
prettyTerm (TermTry expr catchp _finallyp) =
  ScalaDoc
    TermGroupExpr1
    ("try" <+> parensLeft TermGroupExpr (prettyTerm expr) <+> "catch" <+> encloseSep (lbrace <> hardline) (hardline <> rbrace) hardline (indent 2 . pretty <$> catchp))
prettyTerm (TermFunction params body) =
  ScalaDoc
    TermGroupExpr
    (tupled (pretty <$> params) <+> "=>" <+> parensLeft TermGroupExpr (prettyTerm body))
prettyTerm (TermPartialFunction cases) =
  ScalaDoc
    TermGroupSimpleExpr
    ( hardlines
        ( [lbrace]
            <> (indent 2 . pretty <$> cases)
            <> [rbrace]
        )
    )
prettyTerm (TermForYield enums body) =
  ScalaDoc
    TermGroupExpr1
    ("for" <+> hardlines ([lbrace] <> (indent 2 . pretty <$> enums) <> [rbrace]) <+> "yield" <+> pretty body)
prettyTerm (TermNew init') =
  ScalaDoc
    TermGroupSimpleExpr
    ("new" <+> pretty init')
prettyTerm (TermNewAnonymous templ) =
  ScalaDoc
    TermGroupSimpleExpr
    ("new" <+> pretty templ)
prettyTerm TermPlaceholder =
  ScalaDoc
    TermGroupSimpleExpr1
    "_"
prettyTerm (TermEta expr) =
  ScalaDoc
    TermGroupPostfixExpr
    (parensLeft TermGroupSimpleExpr1 (prettyTerm expr) <+> "_")
prettyTerm (TermRepeated expr) =
  ScalaDoc
    TermGroupPostfixExpr
    (parensLeft TermGroupPostfixExpr (prettyTerm expr) <+> ":" <+> "_*")

instance Pretty Term where
  pretty =
    pretty . prettyTerm

parseTerm :: Text -> Object -> MaybeT Parser Term
parseTerm t o =
  case t of
    "Term.Interpolate" ->
      lift
        ( TermInterpolate
            <$> explicitParseField nameParser o "prefix"
            <*> o .: "parts"
            <*> o .: "args"
        )
    "Term.Apply" ->
      lift
        ( TermApply
            <$> o .: "fun"
            <*> o .: "args"
        )
    "Term.ApplyType" ->
      lift
        ( TermApplyType
            <$> o .: "fun"
            <*> o .: "targs"
        )
    "Term.ApplyInfix" ->
      lift
        ( TermApplyInfix
            <$> o .: "lhs"
            <*> explicitParseField nameParser o "op"
            <*> o .: "targs"
            <*> o .: "args"
        )
    "Term.ApplyUnary" ->
      lift
        ( TermApplyUnary
            <$> explicitParseField nameParser o "op"
            <*> o .: "arg"
        )
    "Term.Assign" ->
      lift
        ( TermAssign
            <$> o .: "lhs"
            <*> o .: "rhs"
        )
    "Term.Throw" ->
      lift
        ( TermThrow
            <$> o .: "expr"
        )
    "Term.Ascribe" ->
      lift
        ( TermAscribe
            <$> o .: "expr"
            <*> o .: "tpe"
        )
    "Term.Annotate" ->
      lift
        ( TermAnnotate
            <$> o .: "expr"
            <*> (o .: "annots" >>= listParser (withObject "Mod.Annot" (.: "init")))
        )
    "Term.Tuple" ->
      lift
        ( TermTuple
            <$> o .: "args"
        )
    "Term.Block" ->
      lift
        ( TermBlock
            <$> o .: "stats"
        )
    "Term.If" ->
      lift
        ( TermIf
            <$> o .: "cond"
            <*> o .: "thenp"
            <*> o .: "elsep"
        )
    "Term.Match" ->
      lift
        ( TermMatch
            <$> o .: "expr"
            <*> o .: "cases"
        )
    "Term.Try" ->
      lift
        ( TermTry
            <$> o .: "expr"
            <*> o .: "catchp"
            <*> o .:? "finallyp"
        )
    "Term.Function" ->
      lift
        ( TermFunction
            <$> o .: "params"
            <*> o .: "body"
        )
    "Term.PartialFunction" ->
      lift
        ( TermPartialFunction
            <$> o .: "cases"
        )
    "Term.ForYield" ->
      lift
        ( TermForYield
            <$> o .: "enums"
            <*> o .: "body"
        )
    "Term.New" ->
      lift
        ( TermNew
            <$> o .: "init"
        )
    "Term.NewAnonymous" ->
      lift
        ( TermNewAnonymous
            <$> o .: "templ"
        )
    "Term.Placeholder" ->
      pure TermPlaceholder
    "Term.Eta" ->
      lift
        ( TermEta
            <$> o .: "expr"
        )
    "Term.Repeated" ->
      lift
        ( TermEta
            <$> o .: "expr"
        )
    _ ->
      asum
        [ TermRef
            <$> parseTermRef t o,
          TermLit
            <$> parseLit t o
        ]

instance FromJSON Term where
  parseJSON =
    withTypeObject "Term" parseTerm

data Enumerator
  = EnumeratorGenerator Pat Term
  | EnumeratorVal Pat Term
  --- | EnumeratorGuard Term
  deriving (Eq, Ord, Read, Show)

instance Pretty Enumerator where
  pretty (EnumeratorGenerator pat rhs) =
    pretty pat <+> "<-" <+> pretty rhs
  pretty (EnumeratorVal pat rhs) =
    pretty pat <+> "=" <+> pretty rhs

parseEnumerator :: Text -> Object -> MaybeT Parser Enumerator
parseEnumerator t o =
  case t of
    "Enumerator.Generator" ->
      lift
        ( EnumeratorGenerator
            <$> o .: "pat"
            <*> o .: "rhs"
        )
    "Enumerator.Val" ->
      lift
        ( EnumeratorVal
            <$> o .: "pat"
            <*> o .: "rhs"
        )
    _ ->
      empty

instance FromJSON Enumerator where
  parseJSON =
    withTypeObject "Enumerator" parseEnumerator

data Case
  = Case Pat (Maybe Term) Term
  deriving (Eq, Ord, Read, Show)

instance Pretty Case where
  pretty (Case pat cond body) =
    hardlines
      [ hsep (["case", pretty pat] <> maybe [] (\c -> ["if", pretty c]) cond <> ["=>"]),
        indent 2 (pretty body)
      ]

instance FromJSON Case where
  parseJSON =
    withObject
      "Case"
      ( \o ->
          Case
            <$> o .: "pat"
            <*> o .:? "cond"
            <*> o .: "body"
      )

data TermParam
  = TermParam [Mod] Text (Maybe Type) (Maybe Term)
  deriving (Eq, Ord, Read, Show)

instance Pretty TermParam where
  pretty (TermParam mods name decltpe default') =
    hsep ((pretty <$> mods) <> ([name' <> prettyDecltpe decltpe] <> default''))
    where
      name' =
        if T.null name
          then "_"
          else pretty name
      default'' =
        foldMap (\t -> ["=", pretty t]) default'

instance FromJSON TermParam where
  parseJSON =
    withObject
      "Term.Param"
      ( \o ->
          TermParam
            <$> o .: "mods"
            <*> explicitParseField nameParser o "name"
            <*> o .:? "decltpe"
            <*> o .:? "default"
      )

data TermRef
  = TermRefThis Text
  | TermRefSuper Text Text
  | TermRefName Text
  | TermRefSelect Term Text
  deriving (Eq, Ord, Read, Show)

prettyTermRef :: TermRef -> ScalaDoc TermGroup
prettyTermRef (TermRefThis qual) =
  ScalaDoc
    TermGroupPath
    ( if T.null qual
        then "this"
        else pretty qual <> ".this"
    )
prettyTermRef (TermRefSuper thisp superp) =
  ScalaDoc
    TermGroupPath
    (thisp' <> "super" <> superp')
  where
    thisp' =
      if T.null thisp
        then mempty
        else pretty thisp <> "."
    superp' =
      if T.null superp
        then mempty
        else brackets (pretty superp)
prettyTermRef (TermRefName name) =
  ScalaDoc
    TermGroupPath
    (pretty name)
prettyTermRef (TermRefSelect qual name) =
  ScalaDoc
    TermGroupPath
    (parensLeft TermGroupSimpleExpr (prettyTerm qual) <> "." <> pretty name)

instance Pretty TermRef where
  pretty =
    pretty . prettyTermRef

parseTermRef :: Text -> Object -> MaybeT Parser TermRef
parseTermRef t o =
  case t of
    "Term.This" ->
      lift
        ( TermRefThis
            <$> explicitParseField nameParser o "qual"
        )
    "Term.Super" ->
      lift
        ( TermRefSuper
            <$> explicitParseField nameParser o "thisp"
            <*> explicitParseField nameParser o "superp"
        )
    "Term.Select" ->
      lift
        ( TermRefSelect
            <$> o .: "qual"
            <*> explicitParseField nameParser o "name"
        )
    "Term.Name" ->
      termRefName
    "Name.Indeterminate" ->
      termRefName
    "Name.Anonymous" ->
      termRefName
    _ ->
      empty
  where
    termRefName =
      lift
        ( TermRefName
            <$> nameParser (Object o)
        )

instance FromJSON TermRef where
  parseJSON =
    withTypeObject "Term.Ref" parseTermRef

data Self
  = Self Text (Maybe Type)
  deriving (Eq, Ord, Read, Show)

instance Pretty Self where
  pretty (Self "" Nothing) =
    mempty
  pretty (Self name decltpe) =
    space <> name' <> decltpe' <+> "=>"
    where
      name' =
        if T.null name
          then "this"
          else pretty name
      decltpe' =
        foldMap ((space <>) . (":" <+>) . pretty) decltpe

instance FromJSON Self where
  parseJSON =
    withObject
      "Self"
      ( \o ->
          Self <$> explicitParseField nameParser o "name" <*> o .:? "decltpe"
      )

data Bounds
  = Bounds (Maybe Type) (Maybe Type)
  deriving (Eq, Ord, Read, Show)

instance Pretty Bounds where
  pretty (Bounds lo hi) =
    foldMap ((" >: " <>) . pretty) lo <> foldMap ((" <: " <>) . pretty) hi

instance FromJSON Bounds where
  parseJSON =
    withObject
      "Bounds"
      ( \o ->
          Bounds <$> o .:? "lo" <*> o .:? "hi"
      )

data TypeRef
  = TypeRefName Text
  | TypeRefSelect TermRef Text
  | TypeRefProject Type Text
  | TypeRefSingleton TermRef
  deriving (Eq, Ord, Read, Show)

prettyTypeRef :: TypeRef -> ScalaDoc TypeGroup
prettyTypeRef (TypeRefName value) =
  ScalaDoc
    TypeGroupPath
    (pretty value)
prettyTypeRef (TypeRefSelect ref name) =
  ScalaDoc
    TypeGroupSimpleType
    (pretty ref <> "." <> pretty name)
prettyTypeRef (TypeRefProject qual name) =
  ScalaDoc
    TypeGroupSimpleType
    (parens (pretty qual) <> "#" <> pretty name)
prettyTypeRef (TypeRefSingleton ref) =
  ScalaDoc
    TypeGroupSimpleType
    (pretty ref <> ".type")

instance Pretty TypeRef where
  pretty =
    pretty . prettyTypeRef

parseTypeRef :: Text -> Object -> MaybeT Parser TypeRef
parseTypeRef t o =
  case t of
    "Type.Name" ->
      lift
        ( TypeRefName
            <$> nameParser (Object o)
        )
    "Type.Select" ->
      lift
        ( TypeRefSelect
            <$> o .: "qual"
            <*> explicitParseField nameParser o "name"
        )
    "Type.Project" ->
      lift
        ( TypeRefProject
            <$> o .: "qual"
            <*> explicitParseField nameParser o "name"
        )
    "Type.Singleton" ->
      lift
        ( TypeRefSingleton
            <$> o .: "ref"
        )
    _ ->
      empty

instance FromJSON TypeRef where
  parseJSON =
    withTypeObject "Type.Ref" parseTypeRef

data Ref
  = RefTermRef TermRef
  | RefInit Init
  | RefImportee Importee
  deriving (Eq, Ord, Read, Show)

instance Pretty Ref where
  pretty (RefTermRef termRef) =
    pretty termRef
  pretty (RefInit init') =
    pretty init'
  pretty (RefImportee importee) =
    pretty importee

parseRef :: Text -> Object -> MaybeT Parser Ref
parseRef t o =
  RefTermRef
    <$> parseTermRef t o

instance FromJSON Ref where
  parseJSON =
    withTypeObject "Ref" parseRef

data Importer
  = Importer TermRef [Importee]
  deriving (Eq, Ord, Read, Show)

instance Pretty Importer where
  pretty (Importer ref importees) =
    pretty ref <> "." <> encloseSep lbrace rbrace (comma <> space) (pretty <$> importees)

instance FromJSON Importer where
  parseJSON =
    withObject
      "Importer"
      ( \o ->
          Importer
            <$> o .: "ref"
            <*> o .: "importees"
      )

data Importee
  = ImporteeWildcard
  | ImporteeName Text
  | ImporteeRename Text Text
  | ImporteeUnimport Text
  deriving (Eq, Ord, Read, Show)

instance Pretty Importee where
  pretty ImporteeWildcard =
    "_"
  pretty (ImporteeName name) =
    pretty name
  pretty (ImporteeRename name rename) =
    pretty name <+> "=>" <+> pretty rename
  pretty (ImporteeUnimport name) =
    pretty name <+> "=>" <+> "_"

parseImportee :: Text -> Object -> MaybeT Parser Importee
parseImportee t o =
  case t of
    "Importee.Wildcard" ->
      pure ImporteeWildcard
    "Importee.Name" ->
      lift
        ( ImporteeName
            <$> explicitParseField nameParser o "name"
        )
    "Importee.Rename" ->
      lift
        ( ImporteeRename
            <$> explicitParseField nameParser o "name"
            <*> explicitParseField nameParser o "rename"
        )
    "Importee.Unimport" ->
      lift
        ( ImporteeUnimport
            <$> explicitParseField nameParser o "name"
        )
    _ ->
      empty

instance FromJSON Importee where
  parseJSON =
    withTypeObject "Importee" parseImportee

data Init
  = Init Type [[Term]]
  deriving (Eq, Ord, Read, Show)

instance Pretty Init where
  pretty (Init tpe argss) =
    tpe' <> foldMap (encloseSep lparen rparen (comma <> space) . fmap pretty) argss
    where
      tpe' =
        case tpe of
          TypeRef (TypeRefSingleton _) ->
            "this"
          _ ->
            parensLeft TypeGroupAnnotType (prettyType tpe)

instance FromJSON Init where
  parseJSON =
    withObject
      "Init"
      ( \o ->
          Init
            <$> o .: "tpe"
            <*> o .: "argss"
      )

data Template
  = Template [Stat] [Init] Self [Stat]
  deriving (Eq, Ord, Read, Show)

instance Pretty Template where
  pretty (Template _early inits self stats) =
    hardlines
      ( [inits' <> "{" <> pretty self]
          <> (indent 2 . pretty <$> stats)
          <> ["}"]
      )
    where
      inits' =
        if null inits
          then mempty
          else concatWith (surround " with ") (pretty <$> inits) <> space

instance FromJSON Template where
  parseJSON =
    withObject
      "Template"
      ( \o ->
          Template
            <$> o .: "early"
            <*> o .: "inits"
            <*> o .: "self"
            <*> o .: "stats"
      )

data Mod
  = ModAnnot Init
  | ModPrivate Ref
  | ModProtected Ref
  | ModImplicit
  | ModFinal
  | ModSealed
  | ModOverride
  | ModCase
  | ModAbstract
  | ModCovariant
  | ModContravariant
  | ModLazy
  | ModValParam
  | ModVarParam
  | ModInline
  deriving (Eq, Ord, Read, Show)

instance Pretty Mod where
  pretty (ModAnnot (Init tpe argss)) =
    "@" <> pretty tpe <> foldMap (encloseSep lparen rparen (comma <> space) . fmap pretty) argss
  pretty (ModPrivate (RefTermRef (TermRefName ""))) =
    "private"
  pretty (ModPrivate within) =
    "private" <> brackets (pretty within)
  pretty (ModProtected (RefTermRef (TermRefName ""))) =
    "protected"
  pretty (ModProtected within) =
    "protected" <> brackets (pretty within)
  pretty ModImplicit =
    "implicit"
  pretty ModFinal =
    "final"
  pretty ModSealed =
    "sealed"
  pretty ModOverride =
    "override"
  pretty ModCase =
    "case"
  pretty ModAbstract =
    "abstract"
  pretty ModCovariant =
    "+"
  pretty ModContravariant =
    "-"
  pretty ModLazy =
    "lazy"
  pretty ModValParam =
    "val"
  pretty ModVarParam =
    "var"
  pretty ModInline =
    "inline"

parseMod :: Text -> Object -> MaybeT Parser Mod
parseMod t o =
  case t of
    "Mod.Annot" ->
      lift
        ( ModAnnot
            <$> o .: "init"
        )
    "Mod.Private" ->
      lift
        ( ModPrivate
            <$> o .: "within"
        )
    "Mod.Protected" ->
      lift
        ( ModProtected
            <$> o .: "within"
        )
    "Mod.Implicit" ->
      pure ModImplicit
    "Mod.Final" ->
      pure ModFinal
    "Mod.Sealed" ->
      pure ModSealed
    "Mod.Abstract" ->
      pure ModAbstract
    "Mod.Case" ->
      pure ModCase
    "Mod.Override" ->
      pure ModOverride
    "Mod.Lazy" ->
      pure ModLazy
    "Mod.Covariant" ->
      pure ModCovariant
    "Mod.Contravariant" ->
      pure ModContravariant
    "Mod.ValParam" ->
      pure ModValParam
    "Mod.VarParam" ->
      pure ModVarParam
    "Mod.Inline" ->
      pure ModInline
    _ ->
      empty

instance FromJSON Mod where
  parseJSON =
    withTypeObject "Mod" parseMod

data TypeGroup
  = TypeGroupParamType
  | TypeGroupType
  | TypeGroupAnyInfixType
  | TypeGroupInfixType Text
  | TypeGroupRefineType
  | TypeGroupWithType
  | TypeGroupAnnotType
  | TypeGroupSimpleType
  | TypeGroupPath
  deriving (Eq, Ord, Read, Show)

data TermGroup
  = TermGroupExpr
  | TermGroupExpr1
  | TermGroupPostfixExpr
  | TermGroupInfixExpr Text
  | TermGroupPrefixExpr
  | TermGroupSimpleExpr
  | TermGroupSimpleExpr1
  | TermGroupLiteral
  | TermGroupPath
  deriving (Eq, Ord, Read, Show)

data PatGroup
  = PatGroupPattern
  | PatGroupPattern1
  | PatGroupPattern2
  | PatGroupAnyPattern3
  | PatGroupPattern3 Text
  | PatGroupSimplePattern
  | PatGroupLiteral
  | PatGroupPath
  deriving (Read, Show)

instance Eq PatGroup where
  x == y =
    compare x y == EQ

instance Ord PatGroup where
  compare =
    comparing f
    where
      f :: PatGroup -> Word
      f PatGroupPattern =
        0
      f PatGroupPattern1 =
        1
      f PatGroupPattern2 =
        2
      f PatGroupAnyPattern3 =
        3
      f (PatGroupPattern3 _) =
        4
      f PatGroupSimplePattern =
        5
      f PatGroupLiteral =
        5
      f PatGroupPath =
        5

data ScalaDoc group
  = ScalaDoc group (forall ann. Doc ann)

instance Pretty (ScalaDoc group) where
  pretty (ScalaDoc _ doc) =
    doc

class HasOp group where
  hasOp :: group -> Maybe Text

instance HasOp PatGroup where
  hasOp (PatGroupPattern3 op) =
    Just op
  hasOp _ =
    Nothing

instance HasOp TypeGroup where
  hasOp (TypeGroupInfixType op) =
    Just op
  hasOp _ =
    Nothing

instance HasOp TermGroup where
  hasOp (TermGroupInfixExpr op) =
    Just op
  hasOp _ =
    Nothing

opNeedsParens :: Bool -> Text -> Text -> Bool
opNeedsParens isRhs oo io =
  case compare (namePrecedence oo) (namePrecedence io) of
    EQ ->
      isRhs
    _ ->
      namePrecedence oo > namePrecedence io

parensLeft :: (HasOp g, Ord g) => g -> ScalaDoc g -> Doc ann
parensLeft =
  parens' False

parensRight :: (HasOp g, Ord g) => g -> ScalaDoc g -> Doc ann
parensRight =
  parens' True

parens' :: (HasOp g, Ord g) => Bool -> g -> ScalaDoc g -> Doc ann
parens' isRhs og (ScalaDoc ig doc) =
  if ( case (hasOp og, hasOp ig) of
         (Just oo, Just io) ->
           opNeedsParens isRhs oo io
         _ ->
           og > ig
     )
    then parens doc
    else doc

namePrecedence :: Text -> Word
namePrecedence t =
  case T.uncons t of
    Just (c, _) -> f c
    Nothing -> 0
  where
    f '|' =
      2
    f '^' =
      3
    f '&' =
      4
    f '=' =
      5
    f '!' =
      5
    f '<' =
      6
    f '>' =
      6
    f ':' =
      7
    f '+' =
      8
    f '-' =
      8
    f '*' =
      9
    f '/' =
      9
    f '%' =
      9
    f _ =
      1

prettyDecltpeWithName :: [TypeParam] -> [[TermParam]] -> Text -> Maybe Type -> Doc ann
prettyDecltpeWithName tparams paramss name decltpe =
  ( if null tparams && null paramss
      then foldMap (\(_, c) -> if not (isAlphaNum c) then space else mempty) (T.unsnoc name)
      else mempty
  )
    <> prettyDecltpe decltpe

prettyDecltpe :: Maybe Type -> Doc ann
prettyDecltpe =
  foldMap ((":" <+>) . pretty)

prettyTemplate :: Template -> Doc ann
prettyTemplate templ@(Template _ inits _ _) =
  if null inits
    then pretty templ
    else "extends" <+> pretty templ

hardlines :: [Doc ann] -> Doc ann
hardlines =
  concatWith (surround hardline)

nameParser :: Value -> Parser Text
nameParser =
  withObject "Name" (.: "value")

withTypeObject :: String -> (Text -> Object -> MaybeT Parser a) -> Value -> Parser a
withTypeObject _ f (Object obj) = do
  t <- obj .: "type"
  x <- runMaybeT (f t obj)
  maybe (fail (T.unpack ("unknown type: " <> t))) pure x
withTypeObject expected _ v =
  typeMismatch expected v
