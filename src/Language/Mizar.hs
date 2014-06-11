{-# LANGUAGE DeriveGeneric #-}
module Language.Mizar where

import GHC.Generics

---- Articles

data Article = Article EnvironmentDeclaration TextProper
  deriving (Read, Show, Eq, Ord, Generic)

---- Environments

type EnvironmentDeclaration = [Directive]

data Directive
  = DirectiveVocab [FilePath]
  | DirectiveLib   LibraryDirective
  | DirectiveReq   [FilePath]
  deriving (Read, Show, Eq, Ord, Generic)

data LibraryDirective = LibraryDirective LibType [FilePath]
  deriving (Read, Show, Eq, Ord, Generic)

data LibType
  = Notations
  | Constructors
  | Registrations
  | Definitions
  | Expansions
  | Equalities
  | Theorems
  | Schemes
  deriving (Read, Show, Eq, Ord, Generic)

---- Text Proper

type TextProper = [Section]

type Section = [TextItem]

data TextItem
  = TextItemReservation      Reservation
  | TextItemDefinitionalItem -- DefinitionalItem
  | TextItemRegistrationItem -- RegistrationItem
  | TextItemNotationItem     -- NotationItem
  | TextItemTheorem          -- Theorem
  | TextItemSchemeItem       -- SchemeItem
  | TextItemAuxiliaryItem    AuxiliaryItem
  | TextItemCanceledTheorem  -- CanceledTheorem
  deriving (Read, Show, Eq, Ord, Generic)

data AuxiliaryItem
  = AuxStatement Statement
  | AuxPrivate   -- PrivateDefinition
  deriving (Read, Show, Eq, Ord, Generic)

data Statement
  = StatementLinkable -- LinkableStatement
  | StatementDiffuse  -- DiffuseStatement
  deriving (Read, Show, Eq, Ord, Generic)

type Reservation = [ReservationSegment]

type Identifier = String
type Symbol     = String

data ReservationSegment
  = ReservationSegment [Identifier] TypeExpression
  deriving (Read, Show, Eq, Ord, Generic)

data TypeExpression
  = TypeExpression TypeExpression
  | AdjCluster AdjectiveCluster TypeExpression
  | RadType -- RadixType
  deriving (Read, Show, Eq, Ord, Generic)

type AdjectiveCluster = [Adjective]

data Non = Non
  deriving (Read, Show, Eq, Ord, Generic)

data Adjective
  = Adj (Maybe Non) (Maybe AdjectiveArguments) Symbol
  deriving (Read, Show, Eq, Ord, Generic)

type AdjectiveArguments = [[TermExpression]]

data TermExpression
  = TermExpression        TermExpression
  | TermFuntorSymbol      [TermExpression] Symbol [TermExpression]
  | TermBracket           LeftFunctorBracket [TermExpression] RightFunctorBracket
  | TermFunctorIdentifier Identifier (Maybe [TermExpression])
  | TermStructureSymbol   Symbol [TermExpression]
  | TermVariable          Identifier
  | TermPostqualification TermExpression (Maybe Postqualification) FormulaExpression
  | TermSetOfAll          TermExpression (Maybe Postqualification)
  | TermNumeral           Numeral
  | TermQua               TermExpression TypeExpression
  | TermTheOf             Symbol TermExpression
  | TermTheSymbol         Symbol
  | TermTheType           TypeExpression
  | Term_                 Identifier TypeExpression
  | TermPrivate           PrivateDefinitionParameter
  | TermIt
  deriving (Read, Show, Eq, Ord, Generic)

type Numeral = Integer

data LeftFunctorBracket
  = LeftFunctorSym Symbol
  | LeftFunctorCurelyBrace
  | LeftFunctorSquareBrace
  deriving (Read, Show, Eq, Ord, Generic)

data RightFunctorBracket
  = RightFunctorSym Symbol
  | RightFunctorCurelyBrace
  | RightFunctorSquareBrace
  deriving (Read, Show, Eq, Ord, Generic)

type Postqualification = [PostqualifyingSegment]

data PostqualifyingSegment
  = PS [Identifier] TypeExpression
  deriving (Read, Show, Eq, Ord, Generic)

type PrivateDefinitionParameter = Int

data FormulaExpression
  = FormulaExpression    FormulaExpression
  | FormulaAtomic        AtomicFormulaExpression
  | FormulaQuantified    QuantifiedFormulaExpression
  | FormulaAnd           FormulaExpression FormulaExpression
  | FormulaOr            FormulaExpression FormulaExpression
  | FormulaImplies       FormulaExpression FormulaExpression
  | FormulaIff           FormulaExpression FormulaExpression
  | FormulaNot           FormulaExpression
  | FormulaContradiction
  | FormulaThesis
  deriving (Read, Show, Eq, Ord, Generic)

data AtomicFormulaExpression
  = AtomicPredicateSymbol [TermExpression] Symbol [TermExpression] [(Symbol, [TermExpression])]
  | AtomicIdentifier      Identifier [TermExpression]
  | AtomicIsAdj           TermExpression [Adjective]
  | AtomicIsType          TermExpression TypeExpression
  deriving (Read, Show, Eq, Ord, Generic)

data QuantifiedFormulaExpression
  = QuantifiedFor [QuantifiedVariable] (Maybe FormulaExpression) FormulaExpression
  | QuantifiedEx  [QuantifiedVariable] FormulaExpression
  deriving (Read, Show, Eq, Ord, Generic)

data QuantifiedVariable
  = ImplicitlyQuantified Identifier
  | ExplicitlyQuantified Identifier TypeExpression
  deriving (Read, Show, Eq, Ord, Generic)
