module Utils
where

import Control.Exception (evaluate, Exception, try)
import Data.Either (fromRight)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Map.Strict (adjust, Map)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as THQuote
import Polysemy (Sem)
import Polysemy.Fail (Fail, runFail)
import Text.Parsec hiding (try)


class Pretty a where
  pretty :: a -> String
instance Pretty a => Pretty (Maybe a) where
  pretty (Just a) = pretty a
  pretty Nothing = "⎵"
instance {-# OVERLAPPING  #-} Pretty String where
  pretty = id
instance Pretty Bool where
  pretty b = show $ fromEnum b
{-instance Pretty Rational where
  pretty r = case (numerator r, denominator r) of
               (n, 1) -> show n
               (n, d) -> show n <> "/" <> show d-}
{-instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (k, v) = pretty k <> ": " <> pretty v-}

prettyPrint :: (Pretty a) => a -> IO ()
prettyPrint = putStrLn . pretty

class Pretty1 (f :: * -> *) where
  prettyf :: f String -> String
instance Pretty1 Identity where
  prettyf = runIdentity
instance Pretty1 ((,) meta) where
  prettyf = snd
instance Pretty1 [] where
  prettyf = unlines
instance {-# OVERLAPPABLE #-} (Pretty1 f, Functor f, Pretty a) => Pretty (f a) where
  pretty = prettyf . (pretty <$>)


runFailOr :: forall a r.
             a -> Sem (Fail ': r) a -> Sem r a
runFailOr a = (fromRight a <$>) . runFail

(<$$>) :: forall f g a b. (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
func <$$> struct = (func <$>) <$> struct

(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
func <$$$> struct = (func <$$>) <$> struct

expand :: (Ord k, Semigroup a) => k -> a -> Map k a -> Map k a
expand k a = adjust (<> a) k

-- https://stackoverflow.com/a/17970063/10135377
changeState
  :: forall m s u v a . (Functor m, Monad m)
  => (u -> v)
  -> (v -> u)
  -> ParsecT s u m a
  -> ParsecT s v m a
changeState forward backward = mkPT . transform . runParsecT
  where
    mapState :: forall u' v'.
                (u' -> v') -> State s u' -> State s v'
    mapState f st = st { stateUser = f (stateUser st) }

    mapReply :: forall u' v'.
                (u' -> v') -> Reply s u' a -> Reply s v' a
    mapReply f (Ok a st err) = Ok a (mapState f st) err
    mapReply _ (Error e) = Error e

    fmap3 = fmap . fmap . fmap

    transform
      :: (State s u -> m (Consumed (m (Reply s u a))))
      -> (State s v -> m (Consumed (m (Reply s v a))))
    transform p st = fmap3 (mapReply forward) (p (mapState backward st))


-- https://stackoverflow.com/a/35600656/10135377
throwsException :: forall e a. Exception e => IO a -> IO Bool
throwsException ioa = do eea <- try ioa
                         either (const @(IO Bool) @e (return True)) (
                             fmap (either (const @Bool @e True) (const False)) . try . evaluate
                           ) eea

-- https://stackoverflow.com/a/12717160/10135377
-- Takes one arg: a file name.
litFile :: THQuote.QuasiQuoter
litFile = THQuote.quoteFile $ THQuote.QuasiQuoter{ THQuote.quoteExp = return . TH.LitE . TH.StringL
                                                 , THQuote.quotePat=undefined
                                                 , THQuote.quoteType=undefined
                                                 , THQuote.quoteDec=undefined }

