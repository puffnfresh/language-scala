module Language.Scala.Parser.AST where

data Literal = IntegerLiteral String
             | FloatingPointLiteral String
             | BooleanLiteral Bool
             | CharacterLiteral Char
             | StringLiteral String
             | SymbolLiteral String
             | NullLiteral
               deriving Show

data TopStat = TmplDef TmplDef
             | Import [[String]]
             | Packaging [String]
             | PackageObject TmplDef
             | TopStatEmpty
               deriving Show

data TmplDef = ObjectDef String TemplateBody
             | ClassDef String [String] [[Param]] TemplateBody
             | TraitDef String [String] TemplateBody
               deriving Show

data TemplateBody = TemplateBody [TemplateStat]
                    deriving Show

data TemplateStat = TemplateStatImport [[String]]
                  | TemplateStatDef [Modifier] DefDef
                  | TemplateStatDcl DclDef
                  | TemplateStatExpr Expr
                    deriving Show

data Type = TypeRef [String]
          | TypeTuple [Type]
            deriving Show

data Param = Param String Type
             deriving Show

data Modifier = Abstract
              | Final
              | Sealed
              | Implicit
              | Lazy
                deriving Show

data Match = MatchTuple [Match]
           | MatchId [String]
             deriving Show

data DclDef = DclDef String [String] [[Param]]
            | DclVal String Type
            | DclType String
              deriving Show

data DefDef = DefDef String [String] [[Param]] Expr
            | TypeDef String [String] Type
            | CaseDef Match Expr
              deriving Show

data BlockStat = BlockStatDef DefDef
               | BlockStatImport [[String]]
               | BlockStatTmplDef TmplDef
               | BlockStatExpr Expr
                 deriving Show

data Expr = LiteralExpr Literal
          | ApplyExpr Expr [Expr]
          | SelectExpr Expr String
          | IdentExpr String
          | TypeApplyExpr Expr [Type]
          | BlockExpr [BlockStat]
          | NewExpr TemplateBody
            deriving Show
