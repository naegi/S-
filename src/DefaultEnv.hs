{-# LANGUAGE FlexibleInstances #-}

module DefaultEnv (
    makeEnv
) where


import Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Vals
import Eval

class Arity r where
    arity :: r -> Integer

instance {-# OVERLAPPABLE #-} Arity x where
    arity _ = 0

instance Arity r => Arity ((->) a r) where
    arity f = 1 + arity (f undefined)

class NArgFunc r where
    nargfunc :: r -> SVal -> M SVal

instance {-# OVERLAPPABLE #-} NArgFunc r => NArgFunc ((->) SVal r) where
    nargfunc f a = return $ SValFunc (arity f) SValNone (nargfunc (f a))

instance NArgFunc (SVal -> M SVal) where
    nargfunc f = f
binopInt :: (SNumber -> SNumber -> SNumber) -> String -> (String, SVal)
binopInt op name = (name, SValFunc (arity op) SValNone (nargfunc $ g op))
    where g :: (SNumber -> SNumber -> SNumber) -> SVal -> SVal -> M SVal
          g op (SValNumber a) (SValNumber b) = return $ SValNumber (op a b)
          g op a b = throwError . SEvalError $ "Didn't wait "
            ++ show a ++ " or "
            ++ show b ++ "here"
showEnv :: SVal
showEnv = SValFunc 0 SValNone f
    where f _ = do
            env <- get
            return . SValString . show $ env

showType' :: SVal
showType' = SValFunc 1 SValNone f
    where f = return . SValString . showType

showVar :: SVal
showVar = SValFunc 1 SValNone f
    where f = return . SValString . show

arFunc :: SVal
arFunc = SValFunc 1 SValNone (nargfunc g)
    where g :: SVal -> M SVal
          g (SValFunc n _ f) = return . SValNumber $ SInt n
          g a = throwError . SEvalError $ "Didn't wait " ++ show a ++ "here"


forceApply :: SVal
forceApply = SValFunc 1 SValNone (nargfunc g)
    where g :: SVal -> M SVal
          g (SValFunc _ _ f) = f SValNone
          g a = throwError . SEvalError $ "Didn't wait " ++ show a ++ "here"

makeFunc :: SVal
makeFunc = SValFunc 2 SValNone (nargfunc g)
    where g :: SVal -> SVal -> SVal -> M SVal
          g (SValQuoted idents) c a = funEval (Map.fromList []) idents c a
          g a b c = throwError . SEvalError $ "Didn't wait "
            ++ show a ++ " or "
            ++ show b ++ " or "
            ++ show c ++ " here"

funEval :: MapVal -> [SVal] -> SVal -> SVal -> M SVal
funEval l [] (SValCodeBlock code) a = localEnv l (eval code)
funEval l [SValIdent ident] (SValCodeBlock code) a =
    localEnv (Map.insert ident a l) (eval code)
funEval l (SValIdent ident:idents) c@(SValCodeBlock code) a =
    return $ SValFunc (toInteger (length idents)) c (
    funEval (Map.insert ident a l) idents c)

localEnv :: MapVal -> M SVal -> M SVal
localEnv l f = do
    -- Add local scope
    Env gbls lcs <- get
    put $ Env gbls (l:lcs)
    x <- f
    -- Remove Local scope
    Env gbls (l:lcs) <- get
    put $ Env gbls lcs
    return x


defaultGlobals :: Globals
defaultGlobals = Map.fromList [
    ("ans", SValNone)
  , ("True", SValBool True)
  , ("False", SValBool False)
  , binopInt (+) "+"
  , binopInt (-) "-"
  , binopInt (*) "*"
  --, binopBool (<) "<"
  , ("env", showEnv)
  , ("func", makeFunc)
  , ("show", showVar)
  , ("ap", forceApply)
  , ("arity", arFunc)
  , ("type", showType')
                              ]

makeEnv :: Env
makeEnv = Env defaultGlobals []
