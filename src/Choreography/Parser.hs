{-# OPTIONS_GHC -Wno-orphans #-}

module Choreography.Parser
where

import Data.Functor.Identity (Identity(..))
import Data.List (intercalate, nub)
import Data.Map.Strict ((!?), Map, insert)
import Data.Maybe (isNothing)
import Data.Set (fromList, singleton)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token

import Choreography.AbstractSyntaxTree hiding (owners)
import qualified Choreography.AbstractSyntaxTree as AST
import Choreography.Party
import Utils ((<$$>), Pretty, pretty)


data Location = Location { owners :: PartySet, source :: SourcePos } deriving (Eq, Ord, Show)
type Located = (,) Location
instance Proper Located where
  owners = owners . fst
  value = snd

-- Used so we can first parse an Algebra, and _then_ check if it's valid.
data Improper = Improper { iowners :: Maybe PartySet, isource :: SourcePos } deriving (Eq, Ord, Show)
type ILocated = (,) Improper
asImproper :: Located a -> ILocated a
asImproper (Location{owners, source}, a) = (Improper{iowners=Just owners, isource=source}, a)
asProper :: (Pretty a) => ILocated a -> Parser (Located a)
asProper (Improper{isource=source, iowners=Just owners}, obj) = return (Location{source, owners}, obj)
asProper (Improper{isource=source, iowners=Nothing}, obj) = do setPosition source
                                                               parserFail $ "Unable to locate " ++ pretty obj
                                                                            ++ " among parties; nobody could do that computation."
locateAlgebra :: ILocated (Algebra ILocated) -> Parser (Located (Algebra Located))
locateAlgebra (improper, ialg) = do lalg <- recurse ialg
                                    asProper (improper, lalg)
  where recurse (Literal fb) = Literal <$> asProper fb
        recurse (Var fvar) = Var <$> asProper fvar
        recurse (Xor fa1 fa2) = do la1 <- locateAlgebra fa1
                                   la2 <- locateAlgebra fa2
                                   return $ Xor la1 la2
        recurse (And fa1 fa2) = do la1 <- locateAlgebra fa1
                                   la2 <- locateAlgebra fa2
                                   return $ And la1 la2
        recurse (Not falg) = Not <$> locateAlgebra falg


type Parser = ParsecT String (Map Variable PartySet) Identity
--instance Distributive Parser where
{-distributeF :: (a -> Parser b) -> Parser (a -> b)
distributeF f = mkPT \st@State{stateInput=str, statePos=pos, stateUser=namespace}
                          -> Identity (Empty (Identity (Ok (
                               \a -> case runParsecT (f a) st of
                                 Identity (Consumed (Identity (Ok b st pe))) -> b
                                 Identity (Empty    (Identity (Ok b st pe))) -> b
                             ) st (unknownError st))))-}

positioned :: Parser a -> Parser (SourcePos, a)
positioned p = do source <- getPosition
                  (source,) <$> p

opNames :: [String]
opNames = [bindKeyword, atKeyword, secretKeyword, flipKeyword, outputKeyword]
          <> sendKeywords
          <> oblivKeywords
          <> xorNames
          <> andNames
          <> notNames

tokenizer :: TokenParser st
tokenizer = makeTokenParser LanguageDef {
  commentStart = "{-",
  commentEnd = "-}",
  commentLine = "--",
  nestedComments = False,
  identStart = letter <|> char '_',
  identLetter = lower <|> digit <|> char '_',
  opStart = upper <|> oneOf (nub $ head <$> opNames),
  opLetter = upper <|> digit  <|> oneOf (nub $ concat opNames),
  reservedNames = trueNames <> falseNames,
  reservedOpNames = opNames,
  caseSensitive = True
}

-- Define parser for Party
partyParser :: Parser (Located Party)
partyParser = do (source, _) <- positioned $ char '@'
                 party <- Party <$> identifier tokenizer
                 return (Location{source, owners = Parties $ singleton party}, party)

partiesParser :: Parser (Located PartySet)
partiesParser = do let comma = do {whiteSpace tokenizer; _ <- char ','; whiteSpace tokenizer}
                   (source, owners) <- positioned $ Parties . fromList . (Party <$>) <$> identifier tokenizer `sepBy1` comma
                   return (Location{source, owners}, owners)

boundVariable :: Parser (Located Variable)
boundVariable = do source <- getPosition
                   var <- Variable <$> identifier tokenizer
                   mOwners <- (!? var) <$> getState
                   maybe (parserFail $ "Unbound variable " ++ pretty var ++ ".")
                         (\owners -> return (Location {owners, source}, var))
                         mOwners

-- Define parser for Algebra
algebraParser :: Parser (ILocated (Algebra ILocated))
algebraParser = buildExpressionParser ops terms <?> "Algebra"
  where terms = parens tokenizer algebraParser <|> litParser <|> varParser
        chooseOf cls subcls = choice [positioned . try $ cls tokenizer sc | sc <- subcls] <?> ("one of " ++ intercalate ", " subcls)
        litParser = do (isource, b) <- (const (Bit True) <$$> chooseOf reserved trueNames)
                                       <|> (const (Bit False) <$$> chooseOf reserved falseNames)
                       let loc = Improper{isource, iowners=Just top}
                       return (loc, Literal (loc, b))
        varParser = do (loc, var) <- asImproper <$> boundVariable
                       return (loc, Var (loc, var))
        biOpParser :: (ILocated (Algebra ILocated) -> ILocated (Algebra ILocated) -> Algebra ILocated)
                      -> SourcePos
                      -> ILocated (Algebra ILocated) -> ILocated (Algebra ILocated)
                      -> ILocated (Algebra ILocated)
        biOpParser constructor isource alg1@(Improper{iowners=o1}, _) alg2@(Improper{iowners=o2}, _) =
          (Improper{ isource,
                     iowners = o1 >>= (o2 >>=) . intersect },
           constructor alg1 alg2)
        ops :: OperatorTable String (Map Variable PartySet) Identity (ILocated (Algebra ILocated))
        ops = [ [Prefix $ do (isource, _) <- chooseOf reservedOp notNames
                             return \alg@(loc, _) -> (loc{isource}, Not alg)]
               ,[Infix (do (isource, _) <- chooseOf reservedOp xorNames
                           return $ biOpParser Xor isource) AssocLeft]
               ,[Infix (do (isource, _) <- chooseOf reservedOp andNames
                           return $ biOpParser And isource) AssocLeft] ]

-- Define parser for Expression
expressionParser :: Parser (Located (Statement Located))
expressionParser =  sendParser <|> outputParser <|> bindingParser
  where
    bindingParser :: Parser (Located (Statement Located))
    bindingParser = do (source, boundVar) <- positioned $ Variable <$> identifier tokenizer
                       _ <- reservedOp tokenizer bindKeyword
                       (owners, stmt :: Statement Located) <- choice [
                          -- Secret Parser:
                          do reservedOp tokenizer secretKeyword
                             owner@(Location{owners=os}, _) <- partyParser
                             return (os, Secret (Location{owners=os, source}, boundVar) owner),
                          -- Flip Parser:
                          do reservedOp tokenizer flipKeyword
                             owner@(Location{owners=os}, _) <- partyParser
                             return (os, Flip (Location{owners=os, source}, boundVar) owner),
                          -- Oblivious Transfer Parser:
                          do reservedOp tokenizer (head oblivKeywords)
                             (locC@Location{owners=possiblePTo}, varc) <- boundVariable
                             reservedOp tokenizer (oblivKeywords !! 1)
                             (loc1@Location{owners=o1}, var1) <- boundVariable
                             (loc2@Location{owners=o2}, var2) <- boundVariable
                             let mPFrom = o1 `intersect` o2
                             reservedOp tokenizer (oblivKeywords !! 2)
                             lpTo@(_, pTo) <- partiesParser
                             if not (pTo `isSubsetOf` possiblePTo) then
                               parserFail $ "Parties " ++ pretty pTo ++ " cannot choose choose obliviously by "
                                             ++ pretty varc ++ ", they don't all own it."
                             else if isNothing mPFrom then
                               parserFail $ "Nobody can obliviously send both " ++ pretty var1 ++ " and " ++ pretty var2 ++ "."
                             else do
                               let Just pFrom = mPFrom
                                   lvarc = (locC{owners=pTo}, varc)
                                   lvar1 = (loc1{owners=pFrom}, var1)
                                   lvar2 = (loc2{owners=pFrom}, var2)
                               return (pTo, Oblivious (Location{owners=pTo, source}, boundVar) lpTo lvarc (lvar1, lvar2)),
                          -- Compute Parser:
                          do alg@(Location{owners=os}, _) <- algebraParser >>= locateAlgebra
                             return (os, Compute (Location{owners=os, source}, boundVar) alg)
                        ]
                       modifyState $ insert boundVar owners
                       return (Location{source, owners}, stmt)
    sendParser = do (source, _) <- positioned $ reservedOp tokenizer (head sendKeywords)
                    var@(Location{owners=pFrom}, v) <- boundVariable
                    reservedOp tokenizer (sendKeywords !! 1)
                    lpTo@(_, pTo) <- partiesParser
                    let owners = pFrom `union` pTo
                    modifyState $ insert v owners
                    return (Location {source, owners}, Send lpTo var)
    outputParser = do (source, _) <- positioned $ reservedOp tokenizer outputKeyword
                      var@(loc, _) <- boundVariable
                      return (loc{source}, Output var)

-- Define parser for Program
programParser :: Parser (Program Located)
programParser = do whiteSpace tokenizer
                   lns <-  many (do e <- expressionParser
                                    _ <- whiteSpace tokenizer
                                    return e)
                   eof
                   return lns
