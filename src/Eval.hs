module Eval (
    eval
) where

import Control.Monad.State

import Vals
import Data.Maybe
import Control.Monad.Except


eval :: AST -> M SVal
eval = flip eval' SValNone


eval' :: AST -> SVal -> M SVal
eval' (Root []) v = return v
eval' (Root (x:xs)) v = eval' x v >>= eval' (Root xs)
eval' (Literal a) _ = return a
eval' (Quoted idents) _ = SValQuoted <$> evalQuoted idents
eval' (CodeBlock a) _ = return . SValCodeBlock $ a
eval' (Expr expr) _ = do
    expr' <- mapM eval expr
    x <- evalExpr (flatten expr')
    return $ case x of
      [a] -> a
      xs -> SValList xs

eval' ide@(Identifier ident) _ = do
    maybeVar <- getVar ident
    maybe
        (throwError $ SEvalError $ "Ident " ++ show ide ++ " not identified")
        return maybeVar

eval' (Assign iden ast) _ = do
    val <- eval ast
    setVar False iden val
    return val
eval' _ _ = throwError $ SEvalError "Not implemented yet"


evalQuoted :: [AST] -> M [SVal]
evalQuoted [] = return []
evalQuoted (Identifier ident: ids) = do
    q <- evalQuoted ids
    return $ SValIdent ident:q
evalQuoted _ = throwError . SEvalError $ "Unexpexted Quoted values"

evalExpr :: [SVal] -> M [SVal]
evalExpr (func@(SValFunc _ _ f):a:args) =  f a >>= \x -> evalExpr (x:args)
evalExpr xs = return xs
