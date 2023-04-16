module Choreography.Parser
where

import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity(..))
import Data.List (intercalate, nub)
import Data.Map.Strict ((!), (!?), Map, insert)
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
class ForceLocation alg where
  locate :: alg ILocated -> Parser (alg Located)
forceLocation :: (ForceLocation alg, Pretty (alg Located)) => ILocated (alg ILocated) -> Parser (Located (alg Located))
forceLocation (improper, ialg) = do lalg <- locate ialg
                                    asProper (improper, lalg)
instance ForceLocation Algebra where
  locate (Literal fb) = Literal <$> asProper fb
  locate (Var fvar) = Var <$> asProper fvar
  locate (Xor fa1 fa2) = do la1 <- forceLocation fa1
                            la2 <- forceLocation fa2
                            return $ Xor la1 la2
  locate (And fa1 fa2) = do la1 <- forceLocation fa1
                            la2 <- forceLocation fa2
                            return $ And la1 la2
  locate (Not falg) = Not <$> forceLocation falg
instance ForceLocation ObvBody where
  locate (ObvBody fc0 fc1 fv) = do lc0 <- forceLocation fc0
                                   lc1 <- forceLocation fc1
                                   lv <- asProper fv
                                   return $ ObvBody lc0 lc1 lv
instance ForceLocation ObvChoice where
  locate (ObvLeaf var) = return $ ObvLeaf var
  locate (ObvBranch body) = ObvBranch <$> locate body


type Parser = ParsecT String (Map Variable PartySet) Identity

positioned :: Parser a -> Parser (SourcePos, a)
positioned p = do source <- getPosition
                  (source,) <$> p

opNames :: [String]
opNames = [bindKeyword, atKeyword, secretKeyword, flipKeyword, outputKeyword, choiceKeyword]
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

-- Define parser for Oblivious terms
obvTransferParser :: Parser (ILocated (ObvBody ILocated))
obvTransferParser = do let branchParser = (asImproper <$> (ObvLeaf <$$> boundVariable))
                                          <|> (ObvBranch <$$> obvTransferParser)
                       (isource, [choice0@(o0, _), choice1@(o1, _)]) <- positioned
                                                                        $ brackets tokenizer
                                                                        $ branchParser `sepBy` (char ',' >> whiteSpace tokenizer)
                       reservedOp tokenizer choiceKeyword
                       selectionVar <- boundVariable
                       return (Improper{isource, iowners = iowners o0 >>= (iowners o1 >>=) . intersect},
                               ObvBody choice0 choice1 $ asImproper selectionVar)

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
                             lbody@(_, body) <- obvTransferParser >>= forceLocation
                             reservedOp tokenizer (oblivKeywords !! 1)
                             lpTo@(_, pTo) <- partiesParser
                             traverse_ (\var ->
                                 do possiblePTo <- (! var) <$> getState
                                    if pTo `isSubsetOf` possiblePTo
                                    then return ()
                                    else parserFail $ "Parties " ++ pretty pTo ++ " cannot choose choose obliviously by "
                                                       ++ pretty var ++ "; they don't all own it."
                               ) $ gatherSelectionVars body
                             return (pTo, Oblivious (Location{owners=pTo, source}, boundVar) lpTo lbody),
                          -- Compute Parser:
                          do alg@(Location{owners=os}, _) <- algebraParser >>= forceLocation
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
