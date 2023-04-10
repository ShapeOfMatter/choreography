module Utils
where

import Data.Either (fromRight)

import Polysemy (Members, Sem)
import Polysemy.Error (Error, throw)
import Polysemy.Fail (Fail, runFail)
import Polysemy.Reader (ask, Reader)
import Polysemy.Tagged (tag, Tagged)


type LineNo = Int

throwLn :: forall a k r.
           (Members '[Tagged k (Reader LineNo)
                     ,Error String] r) =>
           String -> Sem r a
throwLn err = do ln <- tag $ ask @LineNo
                 throw $ "[Line " ++ show ln ++ "] " ++ err

runFailOr :: forall a r.
             a -> Sem (Fail ': r) a -> Sem r a
runFailOr a = (fromRight a <$>) . runFail
