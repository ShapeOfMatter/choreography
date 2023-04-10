module Choreography.Parser
where

import Data.List (nub)
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr

import Choreography.EasySyntaxTree
import Choreography.Party
import Text.Parsec.Token

trueNames :: [String]
trueNames = ["1", "true", "True"]
falseNames :: [String]
falseNames = ["0", "false", "False"]
andNames :: [String]
andNames = ["AND", "And", "*", "^", "∧"]
xorNames :: [String]
xorNames = ["XOR", "Xor", "+", "<>", "!=", "⊻", "⊕"]
notNames :: [String]
notNames = ["NOT", "Not", "!", "¬", "~"]
opNames :: [String]
opNames = andNames <> xorNames <> notNames
otherKeywords :: [String]
otherKeywords = ["SECRET", "FLIP", "SEND", "TO", "OUTPUT", "="]

tokenizer :: TokenParser st
tokenizer = makeTokenParser LanguageDef {
  commentStart = "{-",
  commentEnd = "-}",
  commentLine = "--",
  nestedComments = False,
  identStart = lower <|> char '_',
  identLetter = alphaNum <|> char '_',
  opStart = upper <|> oneOf (nub $ head <$> opNames),
  opLetter = alphaNum <|> oneOf (nub $ concat opNames),
  reservedNames = ["0", "1", "true", "false"],
  reservedOpNames = opNames <> otherKeywords,
  caseSensitive = True
}

-- Define parser for Party
partyParser :: Parser Party
partyParser = Party <$> (char '@' >> identifier tokenizer)

-- Define parser for Algebra
algebraParser :: Parser Algebra
algebraParser = buildExpressionParser ops terms
  where terms = parens tokenizer algebraParser <|> litParser <|> varParser
        chooseOf cls subcls = choice $ cls tokenizer <$> subcls
        litParser = Literal <$> (    (chooseOf reserved trueNames >> return True)
                                 <|> (chooseOf reserved falseNames >> return False))
        varParser = Var <$> identifier tokenizer
        ops = [ [Prefix $ chooseOf reservedOp notNames >> return Not]
               ,[Infix (chooseOf reservedOp xorNames >> return Xor) AssocLeft]
               ,[Infix (chooseOf reservedOp andNames >> return And) AssocLeft] ]

-- Define parser for Expression
expressionParser :: Parser Expression
expressionParser =  try annotatedParser <|> unannotatedParser
  where
    unannotatedParser = try secretParser <|> try flipParser <|> try sendParser <|> try outputParser <|> computeParser
    computeParser = Compute <$> algebraParser
    secretParser = reservedOp tokenizer "SECRET" >> return Secret
    flipParser = reservedOp tokenizer "FLIP" >> return Secret
    sendParser = do reservedOp tokenizer "SEND"
                    alg <- algebraParser
                    optional $ reservedOp tokenizer "TO"
                    return $ Send alg
    outputParser = do reservedOp tokenizer "OUTPUT"
                      Output <$> algebraParser
    annotatedParser = do expr <- unannotatedParser <|> parens tokenizer unannotatedParser
                         party <- partyParser
                         return $ Annotated party expr

-- Define parser for Program
programParser :: Parser Program
programParser = (do var <- identifier tokenizer
                    reservedOp tokenizer "="
                    expr <- expressionParser
                    return $ Just (var, expr)
                ) `sepBy`  whiteSpace tokenizer
