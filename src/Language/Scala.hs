{-# LANGUAGE OverloadedStrings #-}

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
import Data.Foldable (asum)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
  ( Doc,
    Pretty (..),
    brackets,
    comma,
    encloseSep,
    hardline,
    hsep,
    indent,
    lbrace,
    line,
    list,
    lparen,
    parens,
    rbrace,
    rparen,
    semi,
    space,
    tupled,
    vsep,
    (<+>),
  )

data Source
  = Source [Stat]
  deriving (Eq, Ord, Read, Show)

instance Pretty Source where
  pretty (Source stats) =
    vsep (pretty <$> stats)

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
  --- | StatPkgObject [Mod] Text Template
  --- | StatCtorSecondary [Mod] Text [[TermParam]] Init [Stat]
  deriving (Eq, Ord, Read, Show)

instance Pretty Stat where
  pretty (StatTerm term) =
    pretty term
  pretty (StatDecl decl) =
    pretty decl
  pretty (StatDefn defn) =
    pretty defn
  pretty (StatImport importers) =
    "import" <+> encloseSep mempty mempty (comma <> space) (pretty <$> importers)
  pretty (StatPkg ref stats) =
    "package" <+> pretty ref <+> encloseSep (lbrace <> line) (line <> rbrace) line (indent 2 . pretty <$> stats)

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
    hsep ((pretty <$> mods) ++ ["val", encloseSep mempty mempty (comma <> space) (pretty <$> pats) <> ":", pretty decltpe])
  pretty (DeclDef mods name tparams paramss decltpe) =
    hsep ((pretty <$> mods) ++ ["def", pretty name <> tparams' <> paramss' <> ": " <> pretty decltpe])
    where
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
    vsep
      [ hsep ((pretty <$> mods) ++ ["val", encloseSep mempty mempty (comma <> space) (pretty <$> pats) <> prettyDecltpe decltpe, "="]),
        indent 2 (pretty body)
      ]
  pretty (DefnDef mods name tparams paramss decltpe body) =
    vsep
      [ hsep ((pretty <$> mods) ++ ["def", pretty name <> tparams' <> paramss' <> prettyDecltpe decltpe, "="]),
        indent 2 (pretty body)
      ]
    where
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
      removeImplicit (TermParam mods' name' decltpe' default') =
        TermParam (filter (/= ModImplicit) mods') name' decltpe' default'
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

instance Pretty Pat where
  pretty (PatLit lit) =
    pretty lit
  pretty (PatTermName name) =
    pretty name
  pretty (PatTermSelect qual name) =
    pretty qual <> "." <> pretty name
  pretty (PatVar name) =
    pretty name
  pretty PatWildcard =
    "_"
  pretty (PatBind lhs rhs) =
    pretty lhs <+> "@" <+> pretty rhs
  pretty (PatAlternative lhs rhs) =
    pretty lhs <+> "|" <+> pretty rhs
  pretty (PatTuple args) =
    tupled (pretty <$> args)
  pretty (PatExtract fun args) =
    pretty fun <+> tupled (pretty <$> args)
  pretty (PatExtractInfix lhs op rhs) =
    pretty lhs <+> pretty op <+> tupled (pretty <$> rhs)
  pretty (PatTyped lhs rhs) =
    pretty lhs <> ":" <+> pretty rhs

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
  | --- | LitChar Word16
    LitLong Int64
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
  | --- | TypeImplicitFunction [Type] Type
    TypeTuple [Type]
  | TypeWith Type Type
  | --- | TypeAnd Type Type
    --- | TypeOr Type Type
    TypeRefine (Maybe Type) [Stat]
  | --- | TypeExistential Type [Stat]
    TypeAnnotate Type [Init]
  | --- | TypeLambda [TypeParam] Type
    --- | TypeMethod [[TypeParam]] Type
    TypePlaceholder Bounds
  | TypeByName Type
  | TypeRepeated Type
  --- | TypeVar Text
  --- | TypeParam' TypeParam
  deriving (Eq, Ord, Read, Show)

instance Pretty Type where
  pretty (TypeRef typeRef) =
    pretty typeRef
  pretty (TypeApply tpe args) =
    pretty tpe <> prettyList args
  pretty (TypeApplyInfix lhs op rhs) =
    parens (pretty lhs <+> pretty op <+> pretty rhs)
  pretty (TypeFunction params res) =
    tupled (pretty <$> params) <+> "=>" <+> pretty res
  pretty (TypeTuple args) =
    tupled (pretty <$> args)
  pretty (TypeWith lhs rhs) =
    pretty lhs <+> "with" <+> pretty rhs
  pretty (TypeRefine tpe stats) =
    pretty tpe <+> encloseSep lbrace rbrace semi (pretty <$> stats)
  pretty (TypeAnnotate tpe annots) =
    pretty tpe <+> hsep (("@" <>) . pretty <$> annots)
  pretty (TypePlaceholder bounds) =
    "_" <> pretty bounds
  pretty (TypeByName tpe) =
    "=>" <+> pretty tpe
  pretty (TypeRepeated tpe) =
    pretty tpe <> "*"

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
  pretty (TypeParam _mods name tparams tbounds _vbounds cbounds) =
    name' <> tparams' <> pretty tbounds <> cbounds'
    where
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
          else ":" <+> encloseSep mempty mempty ": " (pretty <$> cbounds)

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
    --- | TermThrow Term
    TermAscribe Term Type
  | TermAnnotate Term [Init]
  | TermTuple [Term]
  | TermBlock [Stat]
  | TermIf Term Term Term
  | TermMatch Term [Case]
  | --- | TermTry Term [Case] (Maybe Term)
    --- | TermTryWithHandler Term Term (Maybe Term)
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
  --- | TermRepeated Term
  deriving (Eq, Ord, Read, Show)

instance Pretty Term where
  pretty (TermRef termRef) =
    pretty termRef
  pretty (TermInterpolate prefix parts args) =
    pretty prefix <> "\"" <> go parts args <> "\""
    where
      go (LitString p : ps) (a : as) =
        pretty p <> "${" <> pretty a <> "}" <> go ps as
      go (LitString p : _) _ =
        pretty p
      go _ _ =
        mempty
  pretty (TermApplyType fun targs) =
    pretty fun <> targs'
    where
      targs' =
        if null targs
          then mempty
          else list (pretty <$> targs)
  pretty (TermApplyInfix lhs op targs args) =
    parens (pretty lhs <+> pretty op <> targs' <+> args')
    where
      targs' =
        if null targs
          then mempty
          else list (pretty <$> targs)
      args' =
        case args of
          [arg] ->
            pretty arg
          _ ->
            tupled (pretty <$> args)
  pretty (TermApply fun args) =
    pretty fun <> tupled (pretty <$> args)
  pretty (TermApplyUnary op arg) =
    pretty op <> pretty arg
  pretty (TermAssign lhs rhs) =
    pretty lhs <+> "=" <+> pretty rhs
  pretty (TermLit lit) =
    pretty lit
  pretty (TermAscribe expr tpe) =
    parens (pretty expr) <+> ":" <+> pretty tpe
  pretty (TermAnnotate expr annots) =
    parens (pretty expr <> ":" <+> hsep (("@" <>) . pretty <$> annots))
  pretty (TermTuple args) =
    tupled (pretty <$> args)
  pretty (TermBlock stats) =
    vsep
      ( ["{"]
          <> (indent 2 . pretty <$> stats)
          <> ["}"]
      )
  pretty (TermIf cond thenp elsep) =
    vsep
      [ "if (" <> pretty cond <> ")",
        indent 2 (pretty thenp),
        "else",
        indent 2 (pretty elsep)
      ]
  pretty (TermMatch expr cases) =
    pretty expr <+> "match" <+> lbrace <> line <> vsep (indent 2 . pretty <$> cases) <> line <> rbrace
  pretty (TermFunction params body) =
    parens (tupled (pretty <$> params) <+> "=>" <+> pretty body)
  pretty (TermPartialFunction cases) =
    vsep
      ( [lbrace]
          <> (indent 2 . pretty <$> cases)
          <> [rbrace]
      )
  pretty (TermForYield enums body) =
    "for" <+> encloseSep mempty mempty hardline (["{"] <> (indent 2 . pretty <$> enums) <> ["}"]) <+> "yield" <+> pretty body
  pretty (TermNew init') =
    "new" <+> pretty init'
  pretty (TermNewAnonymous templ) =
    "new" <+> pretty templ
  pretty TermPlaceholder =
    "_"
  pretty (TermEta expr) =
    pretty expr <+> "_"

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
  --- | EnumeratorVal Pat Term
  --- | EnumeratorGuard Term
  deriving (Eq, Ord, Read, Show)

instance Pretty Enumerator where
  pretty (EnumeratorGenerator pat rhs) =
    pretty pat <+> "<-" <+> pretty rhs

parseEnumerator :: Text -> Object -> MaybeT Parser Enumerator
parseEnumerator t o =
  case t of
    "Enumerator.Generator" ->
      lift
        ( EnumeratorGenerator
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
    vsep
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
  | --- | TermRefSuper Text Text
    TermRefName Text
  | TermRefSelect Term Text
  deriving (Eq, Ord, Read, Show)

instance Pretty TermRef where
  pretty (TermRefThis qual) =
    if T.null qual
      then "this"
      else "this." <> pretty qual
  pretty (TermRefName name) =
    pretty name
  pretty (TermRefSelect qual name) =
    pretty qual <> "." <> pretty name

parseTermRef :: Text -> Object -> MaybeT Parser TermRef
parseTermRef t o =
  case t of
    "Term.This" ->
      lift
        ( TermRefThis
            <$> explicitParseField nameParser o "qual"
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
  pretty (Self name decltpe) =
    foldMap
      ( \t ->
          space <> name' <+> ":" <+> pretty t <+> "=>"
      )
      decltpe
    where
      name' =
        if T.null name
          then "this"
          else pretty name

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

instance Pretty TypeRef where
  pretty (TypeRefName value) =
    pretty value
  pretty (TypeRefSelect ref name) =
    pretty ref <> "." <> pretty name
  pretty (TypeRefProject qual name) =
    parens (pretty qual) <> "#" <> pretty name
  pretty (TypeRefSingleton ref) =
    pretty ref <> ".type"

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
    pretty tpe <> foldMap (encloseSep lparen rparen (comma <> space) . fmap pretty) argss

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
    vsep
      ( [inits' <> "{" <> pretty self]
          <> (indent 2 . pretty <$> stats)
          <> ["}"]
      )
    where
      inits' =
        if null inits
          then mempty
          else encloseSep mempty mempty " with " (pretty <$> inits) <> space

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
    "Mod.ValParam" ->
      pure ModValParam
    _ ->
      empty

instance FromJSON Mod where
  parseJSON =
    withTypeObject "Mod" parseMod

prettyDecltpe :: Maybe Type -> Doc ann
prettyDecltpe =
  foldMap ((" :" <+>) . pretty)

prettyTemplate :: Template -> Doc ann
prettyTemplate templ@(Template _ inits _ _) =
  if null inits
    then pretty templ
    else "extends" <+> pretty templ

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
