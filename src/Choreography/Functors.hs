module Choreography.Functors where

import Data.Bifunctor (first)
import Data.Functor.Identity (Identity (Identity))
import Text.Parsec (SourcePos)

import Choreography.AbstractSyntaxTree (Algebra(..), ObvBody(..), ObvChoice(..), Statement(..), Program)
import Choreography.Party (PartySet)
import Utils ((<$$>), (<$$$>), Pretty, pretty)

class Proper f where
  owners :: f a -> PartySet
  value :: f a -> a
instance Proper ((,) PartySet) where
  owners = fst
  value = snd

data Location = Location { lowners :: PartySet, source :: SourcePos } deriving (Eq, Ord, Show)
instance Pretty Location where pretty = show
type Located = (,) Location
instance Proper Located where
  owners = lowners . fst
  value = snd

data Improper = Improper { iowners :: Maybe PartySet, isource :: SourcePos } deriving (Eq, Ord, Show)
type ILocated = (,) Improper

class AntiFunctor d where
  antimap :: (Functor f, Functor g) => (forall a. f a -> g a) -> d f -> d g

instance AntiFunctor Algebra where
  antimap t a = case a of
    Literal f -> Literal $ t f
    Var f -> Var $ t f
    Xor f1 f2 -> (antimap t <$> t f1) `Xor` (antimap t <$> t f2)
    And f1 f2 -> (antimap t <$> t f1) `And` (antimap t <$> t f2)
    Not f -> Not $ antimap t <$> t f

instance AntiFunctor ObvBody where
  antimap t (ObvBody f1 f2 fv) = ObvBody (antimap t <$> t f1) (antimap t <$> t f2) (t fv)

instance AntiFunctor ObvChoice where
  antimap _ (ObvLeaf v) = ObvLeaf v
  antimap t (ObvBranch ob) = ObvBranch $ antimap t ob

instance AntiFunctor Statement where
  antimap t s = case s of
    Compute fv fa -> Compute (t fv) (antimap t <$> t fa)
    Secret fv fp -> Secret (t fv) (t fp)
    Flip fv fp -> Flip (t fv) (t fp)
    Send fps fv -> Send (t fps) (t fv)
    Oblivious fv fps fob -> Oblivious (t fv) (t fps) (antimap t <$> t fob)
    Output fv -> Output (t fv)
    Declaration fn args fp -> Declaration (t fn) (first t <$> t <$$$> args) (antimap t <$$> t <$> fp)
    Call fn args gets -> Call (t fn) (first t <$> t <$$$> args) (first t <$> t <$$> gets)


changeFunctor :: (Functor f, Functor g) => (forall a. f a -> g a) -> Program f -> Program g
changeFunctor t p = fmap (antimap t) . t <$> p

removeContext :: Program ((,) a) -> Program Identity
removeContext = changeFunctor $ Identity . snd


