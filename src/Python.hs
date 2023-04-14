module Python where

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Data.List (intercalate)
import GHC.IO.Handle
import System.Exit
import System.Process
import Text.Read (readMaybe)

runPythonCommand :: forall a. (Read a) => String -> IO a
runPythonCommand code = withCreateProcess
  (shell $ "python -c \"" ++ code ++ "\""){std_err=Inherit, std_out=CreatePipe}
  (\Nothing (Just stdout_hdl) Nothing ph -> do
     output <- hGetContents stdout_hdl
     exitCode <- waitForProcess ph
     evaluate $ rnf output  -- Unclear if this is actually doing anything.
     case exitCode of
       ExitSuccess -> maybe (ioError $ userError $ "Couldn't parse " ++ show output) return $ readMaybe @a output
       failure@(ExitFailure _) -> ioError $ userError $ "Python terminated with " ++ show failure
  )

newtype Library = PythonLibrary {library :: String} deriving (Eq, Ord, Show)

healthCheck :: [Library] -> IO ()
healthCheck libs = do let code = intercalate "\n" (("import " ++) . library <$> libs) ++ "\nprint(5)"
                      value <- runPythonCommand @Int code
                      case value of
                        5 -> return ()
                        _ -> ioError $ userError "Unknown problem with your python environment. Hopefully this isn't the only error message?"

