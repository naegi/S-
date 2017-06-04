import System.Console.Haskeline as H
import Data.List (isPrefixOf)
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Morph (hoist)
import Control.Monad.Identity (Identity(..))
import Data.Map as Map (keys, insert, unions)

import Vals
import Parse
import Eval
import DefaultEnv

interpret ::  String -> M SVal
interpret str = parseS "" str >>= eval

runEM = runStateT . runExceptT

defaultCompl :: [String]
defaultCompl = ["'("]

findCompletion :: String -> StateT Env IO [Completion]
findCompletion s = do 
    (Env gbls lcs) <- get
    return $ Prelude.map simpleCompletion $ filter (isPrefixOf s) (Map.keys (Map.unions (gbls:lcs)) ++ defaultCompl)

settings :: Settings (StateT Env IO)
settings = setComplete (H.completeWord Nothing " \t" findCompletion) H.defaultSettings

repl :: InputT (StateT Env IO)()
repl = do
        maybeLine <- H.getInputLine "S~> "
        case maybeLine of 
            Nothing     -> return () -- EOF / control-d
            Just "exit" -> return ()
            Just line -> do
                env <- lift get
                let (val, env) = runIdentity $ runEM (interpret line) env
                lift $ put env
                val <- either report printifnotnone val
                ans val
                repl

printifnotnone :: SVal -> InputT (StateT Env IO) SVal
printifnotnone SValNone = return SValNone
printifnotnone a = H.outputStrLn (show a) >> return a

ans :: SVal -> InputT (StateT Env IO) ()
ans SValNone = return ()
ans a = do 
    (Env gbls lcs) <- lift get
    lift $ put $ Env (Map.insert "ans" a gbls) lcs

report :: SError -> InputT (StateT Env IO) SVal
report err = H.outputStrLn (show err) >> return SValNone

main = runStateT (runInputT settings repl) makeEnv
