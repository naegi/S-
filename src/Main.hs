import System.Console.Haskeline as H
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Morph (hoist)
import Control.Monad.Identity (Identity(..))
import Data.Map as Map (unions, keys)
import Data.List (isPrefixOf)
import Control.Applicative
import Vals
import Parse
import Eval
import DefaultEnv


unwrapIO = hoist (hoist generalize)

generalize :: Monad m => Identity a -> m a
generalize = return . runIdentity

interpret ::  String -> M SVal
interpret str = parseS "" str >>= eval

defaultCompl :: [String]
defaultCompl = ["'(", "exit"]

settings :: Settings (E (StateT Env IO))
settings = setComplete (H.completeWord Nothing " \t" findCompletion) H.defaultSettings

findCompletion :: String -> E (StateT Env IO) [Completion]
findCompletion s = do
    (Env gbls lcs) <- get
    return $ Prelude.map simpleCompletion $ filter (isPrefixOf s) (Map.keys (Map.unions (gbls:lcs)) ++ defaultCompl)

instance H.MonadException m => H.MonadException (ExceptT e m) where
    controlIO f = ExceptT $ controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap ExceptT . run . runExceptT)
                 in runExceptT <$> f run'

instance MonadException m => MonadException (StateT s m) where
    controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
                    in (`runStateT` s) <$> f run'

repl :: InputT (E (StateT Env IO)) ()
repl = do
    maybeLine <- H.getInputLine "S~> "
    case maybeLine of 
        Nothing     -> return () -- EOF / control-d
        Just line -> do
            val <- lift $ unwrapIO (interpret line)
                        `catchError` report
            liftIO $ printifnotnone val
            lift $ unwrapIO $ ans val
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
main = void $ runStateT (runExceptT $ runInputT settings repl) makeEnv
