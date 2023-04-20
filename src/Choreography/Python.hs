{-# LANGUAGE QuasiQuotes #-}
module Choreography.Python where

import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.List (intercalate, elemIndex)
import Data.Map.Strict ((!), empty, foldMapWithKey, fromList, Map, singleton, unionWith)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Polysemy (Members, run, Sem)
import Polysemy.Writer (runWriter, tell, Writer)
import Polysemy.State (gets, modify, runState, State)

import Choreography.AbstractSyntaxTree
import Choreography.Party
import Choreography.Semantics (Inputs, inputsMap, Outputs (Outputs), Tapes, tapesMap, Views(Views))
import Python
import Utils (litFile, pretty, Pretty1)

numpy :: String -> PythonExpression
numpy = ("np." ++)

n :: PythonExpression
n = "n"

header :: PythonLines
header = pythonLines [litFile|snippits/header.py|]

data ProgramMetaData = ProgramMetaData { inputVars :: Map Party [Variable]
                                       , tapeVars :: Map Party [Variable]
                                       , viewVars :: Map Party [Variable]
                                       , outputVars :: Map Party [Variable]
                                       } deriving (Eq, Ord, Show)
instance Semigroup ProgramMetaData where
  ProgramMetaData ivs1 tvs1 vvs1 ovs1 <>  ProgramMetaData ivs2 tvs2 vvs2 ovs2 =
    ProgramMetaData (unionWith (<>) ivs1 ivs2) (unionWith (<>) tvs1 tvs2) (unionWith (<>) vvs1 vvs2) (unionWith (<>) ovs1 ovs2)
instance Monoid ProgramMetaData where
  mempty = ProgramMetaData empty empty empty empty
metadata :: ProgramMetaData
metadata = mempty

at :: (Proper f) => f Variable -> f Party -> Map Party [Variable]
fv `at` fp = singleton (value fp) [value fv]
ats :: (Proper f) => f Variable -> PartySet -> Map Party [Variable]
fv `ats` ps = fromList [(p, [value fv]) | p <- Set.toList . parties $ ps]
registrationIndex :: (Members '[State ProgramMetaData] r) =>
                     (ProgramMetaData -> Map Party [Variable]) -> ProgramMetaData -> Sem r Int
registrationIndex func meta = do modify (<> meta)
                                 gets $ (-1 +) . sum . fmap length . func

pySemantics :: forall f r.
             (Members '[
                 Writer PythonLines
                ,State ProgramMetaData
               ] r,
              Proper f, Functor f, Traversable f, Pretty1 f) =>
             Program f -> Sem r ()
pySemantics = traverse_ pyStatement

pyStatement :: forall f r.
             (Members '[
                 Writer PythonLines
                ,State ProgramMetaData
               ] r,
              Proper f, Functor f, Traversable f, Pretty1 f) =>
             f (Statement f) -> Sem r ()
pyStatement fs = do
  case value fs of
    (Compute fv falg) -> bindVar fv $ pyAlg falg
    (Secret fv fp) -> do i <- registrationIndex inputVars $ metadata{inputVars = fv `at` fp}
                         bindVar fv $ "inputs" `index` [show i]
    (Flip fv fp) -> do i <- registrationIndex tapeVars $ metadata{tapeVars = fv `at` fp, viewVars= fv `at` fp}
                       bindVar fv $ "tapes" `index` [show i]
    (Send fps fv) -> do modify (<> metadata{viewVars = fv `ats` value fps})
                        tell . comment . pretty $ value fs
    (Output fv) -> do modify (<> metadata{outputVars = fv `ats` owners fv})
                      tell . comment . pretty $ value fs
    (Oblivious fv fps fbody) -> do modify (<> metadata{viewVars = fv `ats` value fps})
                                   bindVar fv $ pyObliv $ value fbody
  where bindVar fv body = tell $ pythonLines $ variable (value fv) ++ " = " ++ body

pyAlg :: (Proper f) => f (Algebra f) -> PythonExpression
pyAlg = pyAlg' . value
  where pyAlg' (Literal fb) = numpy (bool "zeros" "ones" $ bit $ value fb) `apply` [n]
        pyAlg' (Var fv) = variable . value $ fv
        pyAlg' (Xor fa1 fa2) = "xor" `apply` [pyAlg fa1, pyAlg fa2]
        pyAlg' (And fa1 fa2) = "land" `apply` [pyAlg fa1, pyAlg fa2]
        pyAlg' (Not falg) = "xor" `apply` [pyAlg falg, numpy "ones" `apply` [n]]

pyObliv :: (Proper f) => ObvBody f -> PythonExpression
pyObliv (ObvBody fc0 fc1 fv) = numpy "choose" `apply` [variable $ value fv, "" `apply` [obvChoice $ value fc0, obvChoice $ value fc1]]
  where obvChoice (ObvLeaf var) = variable var
        obvChoice (ObvBranch body) = pyObliv body


data PyTrace = PyTrace { inputs :: [Int], tapes :: [Int], outputs :: [Int], views :: [Int] } deriving (Eq, Ord, Read, Show)

asPythonFunction :: (Proper f, Functor f, Traversable f, Pretty1 f) =>
                    PartySet -> Program f -> (PythonLines, ProgramMetaData)
asPythonFunction ps prog = (pythonLines [litFile|snippits/procedure_signature.py|]
                            <> indent 1 pyLines
                            <> indent 1 gather
                            <> pythonLines [litFile|snippits/procedure_return.py|]
                           , pmd)
  where (pyLines, (pmd@ProgramMetaData{inputVars, tapeVars, viewVars, outputVars}, ()))
          = run . runWriter . runState mempty $ pySemantics prog
        gather = pythonLines $ "selected_inputs = [" ++ psFilter inputVars ++ "]\n\
                               \selected_tapes = [" ++ psFilter tapeVars ++ "]\n\
                               \selected_views = [" ++ psFilter viewVars ++ "]\n\
                               \selected_outputs = [" ++ psFilter outputVars ++ "]"
        psFilter :: Map Party [Variable] -> String
        psFilter mpsv = intercalate ", " . fmap variable
                          $ foldMapWithKey (\p vs -> if p `isElementOf` ps then vs else mempty) mpsv

asPythonProgram :: (Proper f, Functor f, Traversable f, Pretty1 f) => Program f -> Inputs -> Tapes -> (PythonLines, ProgramMetaData)
asPythonProgram prog ins tps = (header
                                <> function
                                <> setup
                                <> pythonLines [litFile|snippits/footer.py|]
                               , pmd)
  where (function, pmd@ProgramMetaData{inputVars, tapeVars}) = asPythonFunction top prog
        setup = pythonLines $ "inputs = np.array([" ++ intercalate ", " [pretty $ inputsMap ins ! var | var <- concat inputVars] ++ "])\n\
                              \tapes = np.array([" ++ intercalate ", " [pretty $ tapesMap tps ! var | var <- concat tapeVars] ++ "])"

runPythonProgram :: (Proper f, Functor f, Traversable f, Pretty1 f) => Program f -> Inputs -> Tapes -> IO (Outputs, Views, PythonLines)
runPythonProgram prog ins tps = do PyTrace{outputs, views} <- runPythonCommand @PyTrace code
                                   return (Outputs $ outputs `arrange` outputVars, Views $ views `arrange` viewVars, code)
  where (code, ProgramMetaData{outputVars, viewVars}) = asPythonProgram prog ins tps
        arrange :: [Int] -> Map Party [Variable] -> Map Party (Map Variable Bool)
        vals `arrange` mapping = let vars = concat mapping
                                 in fromList . ((\v -> (v, toEnum $ vals !! fromJust (elemIndex v vars))) <$>) <$> mapping
