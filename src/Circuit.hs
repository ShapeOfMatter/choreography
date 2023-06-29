module Circuit where

import Data.Bifunctor (first)
import Data.List (findIndex, intercalate)
import Data.Stream (fromList, Stream(Cons))
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary, arbitrary, chooseAny, chooseInt, elements, Gen, genericShrink, getSize, resize, scale, sized, shrink, resize, vectorOf, oneof, infiniteListOf, listOf)

import Utils ((<$$>), Pretty, pretty)

type NodeName = String

infixr 5 :::

data Circuit where
  Reference :: NodeName -> Circuit
  Constant :: Bool -> Circuit
  (:&:) :: Circuit -> Circuit -> Circuit
  (:+:) :: Circuit -> Circuit -> Circuit
  deriving (Generic, Show)

prettyParens :: Circuit -> String
prettyParens (Reference name) = name
prettyParens (Constant val) = show $ fromEnum val
prettyParens c = "(" ++ pretty c ++ ")"

instance Pretty Circuit where
  pretty (a :&: b) = prettyParens a ++ " ∧ " ++ prettyParens b
  pretty (a :+: b) = prettyParens a ++ " ⊻ " ++ prettyParens b
  pretty r = prettyParens r

arbitraryCircuitWith :: [NodeName] -> Gen Circuit
arbitraryCircuitWith names = do size <- getSize
                                if 1 >= size
                                  then oneof $ (Constant <$> chooseAny) : [Reference <$> elements names | null names]
                                  else do left <- chooseInt (1, size)
                                          a <- resize left $ arbitraryCircuitWith names
                                          b <- resize (1 `max` (size - left)) $ arbitraryCircuitWith names
                                          op <- elements [(:&:), (:+:)]
                                          return $ a `op` b

arbitraryNodeName :: Gen NodeName
arbitraryNodeName = ("v" ++) . show <$> sized (curry chooseInt 0)

instance Arbitrary Circuit where
  shrink = genericShrink
  arbitrary = sized (`vectorOf` arbitraryNodeName) >>= arbitraryCircuitWith


data Circuits where
  Nil :: Circuits
  (:::) :: Circuit -> Circuits -> Circuits
  Let :: [NodeName] -> Circuits -> Circuits -> Circuits
  deriving (Generic, Show)

instance Pretty Circuits where
  pretty Nil = "()"
  pretty (c ::: Nil) = prettyParens c
  pretty (c ::: cs) = "(" ++ pretty c ++ ", " ++ deparens (pretty cs) ++ ")"
    where deparens [] = []
          deparens char@[_] = char
          deparens str = let stripLeft = fromMaybe 0 $ findIndex (/= '(') str
                             stripRight = fromMaybe 0 $ findIndex (/= ')') (reverse str)
                             strip = stripLeft `min` stripRight
                         in drop strip $ take (length str - strip) str
  pretty (Let names values body) = "let " ++ intercalate ", " names ++ " = " ++ pretty values ++ " in " ++ pretty body

junkCircuitsAST :: Gen Circuits
junkCircuitsAST = do size <- getSize
                     sizes <- fromList <$> infiniteListOf (chooseInt (1, (size `div` 2) + 1))
                     junk sizes
  where junk :: Stream Int -> Gen Circuits
        junk sizes = do size <- getSize
                        if 1 >= size then return Nil
                                     else oneof [tuple sizes, letIn sizes]
        tuple (s `Cons` sizes) = do c <- scale (min s) (arbitrary @Circuit)
                                    cs <- scale (max 1 . flip (-) s) $ junk sizes
                                    return $ c ::: cs
        letIn (s `Cons` sizes) = do names <- listOf arbitraryNodeName
                                    values <- scale (min s) junkCircuitsAST
                                    body <- scale (max 1 . flip (-) s) $ junk sizes
                                    return $ Let names values body

arbitraryCleanAST :: Gen Circuits
arbitraryCleanAST = outerBuild []
  where outerBuild sigma = do size <- getSize
                              sizes <- fromList <$> infiniteListOf (chooseInt (1, (size `div` 2) + 1))
                              build sigma sizes
        build :: [NodeName] -> Stream Int -> Gen Circuits
        build sigma sizes = do size <- getSize
                               if 1 >= size then return Nil
                                            else oneof [tuple sigma sizes, letIn sigma sizes]
        tuple sigma (s `Cons` sizes) = do c <- scale (min s) $ arbitraryCircuitWith sigma
                                          cs <- scale (max 1 . flip (-) s) $ build sigma sizes
                                          return $ c ::: cs
        letIn sigma (s `Cons` sizes) = do values <- scale (min s) $ outerBuild sigma
                                          let names = ["v" ++ show n | n <- [length sigma .. length sigma + tupleLength values]]
                                          body <- scale (max 1 . flip (-) s) $ build (names ++ sigma) sizes
                                          return $ Let names values body

instance Arbitrary Circuits where
  shrink = genericShrink
  arbitrary = oneof [junkCircuitsAST, arbitraryCleanAST]


tupleLength :: Circuits -> Int
tupleLength Nil = 0
tupleLength (_ ::: cs) = 1 + tupleLength cs
tupleLength (Let _ _ body) = tupleLength body

validate :: [NodeName] -> Circuit -> Bool
validate gamma (Reference name) = name `elem` gamma
validate _ (Constant _) = True
validate gamma (a :&: b) = validate gamma a && validate gamma b
validate gamma (a :+: b) = validate gamma a && validate gamma b

validations :: [NodeName] -> Circuits -> Bool
validations _ Nil = True
validations gamma (c ::: cs) = validate gamma c && validations gamma cs
validations gamma (Let names values body) = ("" /=) `all` names
                                            && length names == tupleLength values
                                            && validations gamma values
                                            && validations (names ++ gamma) body

basicEvaluation :: [(NodeName, Bool)] -> Circuit -> Maybe Bool
basicEvaluation sigma (Reference name) = name `lookup` sigma
basicEvaluation _ (Constant val) = Just val
basicEvaluation sigma (a :&: b) = (&&) <$> basicEvaluation sigma a <*> basicEvaluation sigma b
basicEvaluation sigma (a :+: b) = (/=) <$> basicEvaluation sigma a <*> basicEvaluation sigma b

basicEvaluations :: [(NodeName, Bool)] -> Circuits -> [Maybe Bool]
basicEvaluations _ Nil = []
basicEvaluations sigma (c ::: cs) = basicEvaluation sigma c : basicEvaluations sigma cs
basicEvaluations sigma (Let names values body) = let valuations = basicEvaluations sigma values
                                                     maybeBindings = names `zip` valuations
                                                     bindings = sequenceA `mapMaybe` maybeBindings
                                                 in if length valuations == length names && isJust `all` valuations
                                                      then basicEvaluations (bindings ++ sigma) body
                                                      else [Nothing]



halfAdder :: Circuit -> Circuit -> Circuits
halfAdder a b = (a :+: b) ::: (a :&: b) ::: Nil

fullAdder :: Circuit -> Circuit -> Circuit -> Circuits
fullAdder a b carry = Let [mainSum, mainCarry] (halfAdder a b) (
    Let [carrySum, carryCarry] (halfAdder (Reference mainSum) carry)
    $ Reference carrySum ::: (Reference mainCarry :+: Reference carryCarry) ::: Nil
  )
  where mainSum = "mainSum"; mainCarry = "mainCarry"; carrySum = "carrySum"; carryCarry = "carryCarry"

adder :: [(NodeName, NodeName)] -> Circuits
adder = adder' Nothing . (Reference <$$>) . (first Reference <$>)
  where adder' :: Maybe Circuit -> [(Circuit, Circuit)] -> Circuits
        adder' Nothing [] = Nil
        adder' (Just c) [] = c ::: Nil
        adder' mc ((a, b):ins) = Let [lsb, carry] (bitAdder a b mc) $ Reference lsb ::: adder' (Just $ Reference carry) ins
        bitAdder a b Nothing = halfAdder a b
        bitAdder a b (Just c) = fullAdder a b c
        lsb = "lstSigBit"; carry = "carry"

