{-# LANGUAGE GADTs #-}

module Vals (
    SNumber(..),
    SVal(..),
    AST(..),
    Ident,
    flatten,
    Env(..),
    SError(..), M, E,
    setVar,
    getVar,
    Locals,
    MapVal,
    Globals,
    showType
            ) where
import Control.Monad.State
import Control.Applicative
import Text.Parsec (ParseError)
import Control.Monad.Except
import qualified Data.Map as Map
import Data.List

type Ident = String

data AST = Root [AST]
         | Expr [AST]
         | Quoted [AST]
         | CodeBlock AST
         | Assign Ident AST
         | Identifier Ident
         | Literal SVal
         deriving(Show)

data SNumber = SInt Integer | SDouble Double

instance Show SNumber where
    show (SInt i) = show i
    show (SDouble i) = show i

instance Num SNumber where
    negate (SInt i) = SInt (negate i)
    negate (SDouble i) = SDouble (negate i)

    (SInt a) + (SInt b) = SInt (a + b)
    (SDouble a) + (SDouble b) = SDouble (a + b)
    a@(SDouble _) + (SInt b) = a + SDouble (fromInteger b)
    (SInt a) + b@(SDouble _) = SDouble (fromInteger a) + b

    (SInt a) - (SInt b) = SInt (a - b)
    (SDouble a) - (SDouble b) = SDouble (a - b)
    a@(SDouble _) - (SInt b) = a - SDouble (fromInteger b)
    (SInt a) - b@(SDouble _) = SDouble (fromInteger a) - b

    (SInt a) * (SInt b) = SInt (a * b)
    (SDouble a) * (SDouble b) = SDouble (a * b)
    a@(SDouble _) * (SInt b) = a * SDouble (fromInteger b)
    (SInt a) * b@(SDouble _) = SDouble (fromInteger a) * b

    fromInteger = SDouble . fromInteger
    abs (SInt a) = SInt (abs a)
    abs (SDouble a) = SDouble (abs a)
    signum (SInt a) = SInt (signum a)
    signum (SDouble a) = SDouble (signum a)


data SType = Intersect SVal SVal

data SVal = SValNumber SNumber
          | SADT SType
          | SValFunc Integer SVal (SVal -> M SVal)
          | SValString String
          | SValBool Bool
          | SValIdent String
          | SValQuoted [SVal] -- Mainly Idents
          | SValList [SVal]
          | SValCodeBlock AST
          | SValNone

flatten :: [SVal] -> [SVal]
flatten [] = []
flatten (SValList a:xs) = a ++ flatten xs
flatten (a:xs) = a : flatten xs

class ShowType t where
    showType :: t -> String

instance ShowType SNumber where
    showType (SInt _) = "Int"
    showType (SDouble _) = "Double"

instance ShowType SVal where
    showType (SValNumber n) = showType n
    showType (SValString s) = "String"
    showType (SValIdent s) = "Ident"
    showType (SValList xs) = "List"
    showType (SValFunc args code _) = "Function"
    showType (SValCodeBlock c) = "Code block"
    showType (SValBool b) = "Bool"
    showType SValNone = "None"
    showType (SValQuoted xs) = "Quoted"

showVal (SValNumber n) = show n
showVal (SValString s) = "\"" ++ s ++ "\""
showVal (SValIdent s) = s
showVal (SValList xs) = show xs
showVal (SValFunc args code _) = "<function: "
    ++ show args ++ " "
    ++ show code
    ++ " args>"
showVal (SValCodeBlock c) = "<code block: " ++ show c ++ " >"
showVal (SValBool b) = show b
showVal SValNone = "()"
showVal (SValQuoted xs) = "'(" ++ sxs xs ++ ")"
    where sxs xs = unwords $ map show xs

instance Show SVal where
    show n = showType n ++ " :: " ++ showVal n

              -- Env stuff {{

data SError = SEvalError String | SParseError ParseError-- {{{}}}

instance Show SError where
    show (SEvalError s) = "Eval error: " ++ s
    show (SParseError p) = "Parse Error: " ++ show p

type E = ExceptT SError
type M a = ExceptT SError (State Env) a

type MapVal = Map.Map String SVal

type Globals = MapVal
type Locals = MapVal

data Env = Env Globals [Locals]

instance Show Env where
    show (Env gbls lcs) = "Globals:\n" ++ showScope (Map.toList gbls)
            ++ "Locals: "
            ++ concatMap (showScope . Map.toList) lcs
                where showScope [] = ""
                      showScope ((name, val):xs) = name ++ " -> " ++ show val ++ "\n" ++ showScope xs

getVar :: String -> M (Maybe SVal)
getVar iden = do
    (Env gbls lcs) <- get
    return $ maybeGet iden lcs gbls
        where
            maybeGet :: String -> [Locals] -> Globals -> Maybe SVal
            maybeGet _ [] gbls = Map.lookup iden gbls
            maybeGet iden (x:xs) gbls = maybe (maybeGet iden xs gbls) return (Map.lookup iden x)

setVar :: Bool -> String -> SVal -> M ()
setVar global iden val= do
    env <- get
    f global env
        where f :: Bool -> Env -> M ()
              f True (Env gbls lcs) = put $ Env (Map.insert iden val gbls) lcs
              f False (Env gbls (l:ls)) = put $ Env gbls $ Map.insert iden val l :ls
              f False env = f True env -- If no local put in global

-- }}
