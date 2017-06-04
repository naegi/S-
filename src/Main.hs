import System.Console.Readline as R
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Morph (hoist)
import Control.Monad.Identity (Identity(..))
import Data.Map as M

import Vals
import Parse
import Eval
import DefaultEnv

unwrapIO = hoist (hoist generalize)

generalize :: Monad m => Identity a -> m a
generalize = return . runIdentity

interpret ::  String -> M SVal
interpret str = parseS "" str >>= eval


repl :: E (StateT Env IO) ()
repl = do
    maybeLine <- liftIO $ R.readline "S~> "
    case maybeLine of 
        Nothing     -> return () -- EOF / control-d
        Just "exit" -> return ()
        Just line -> do
                liftIO $ R.addHistory line
                val <- unwrapIO (interpret line)
                        `catchError` report
                liftIO $ printifnotnone val
                unwrapIO $ ans val
                repl

printifnotnone :: SVal -> IO ()
printifnotnone SValNone = return ()
printifnotnone a = print a 

ans :: SVal -> M ()
ans SValNone = return ()
ans a = setVar True "ans" a

report :: SError -> E (StateT Env IO) SVal
report err = liftIO (print err) >> return SValNone

main :: IO ()
main = void $ runStateT (runExceptT repl) makeEnv
