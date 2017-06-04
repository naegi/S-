module Parse (parseS) where

import Control.Monad
import Data.Bifunctor
import Control.Monad.Except
import Text.Parsec
import qualified Text.Parsec.Token as P

import Vals

parseS :: SourceName -> String -> M AST
parseS src str = either throwError return (first SParseError $ parse mainParser src str)

-- Parser

assign = try $ do
    ident <- identifier
    reservedOp "="
    ex <- expr
    return $ Assign ident ex

litNone = reserved "None" >> return SValNone
    <?> "None"

litNumber = try float <|> integer
        <?> "Number"

literal = Literal <$> try (litNone <|> litNumber)

endOfExpr = reservedOp ";"

{- symbolExpr = try $ do
    id1 <- try literal <|> Identifier <$> identifier
    symbol <- Identifier <$> operator
    id2 <- try literal <|> Identifier <$> identifier
    Expr expr <- expr' <|> return (Expr [])
    return $ Expr (symbol : id1 : id2 : expr) -}
expr' = Expr <$> many1 (
        (reservedOp "$" >> expr')
        <|> parens expr'
        <|> try literal 
        <|> (Identifier <$> identifier)
        <|> Quoted <$> quotedParens (many $ Identifier <$> identifier)
        <|> CodeBlock <$> braces mainParser
              )

expr = parens expr
    <|> assign
    <|> expr'

mainParser = whiteSpace >> Root <$> expr `sepEndBy` semi


-- Lexer

sDef :: P.LanguageDef st
sDef = P.LanguageDef
    { P.commentStart   = "/*"
      , P.commentEnd     = "*/"
      , P.commentLine    = "#"
      , P.nestedComments = True
      , P.identStart     = letter <|> char '_'
      , P.identLetter    = alphaNum <|> P.opStart sDef <|> oneOf "'"
      , P.opStart        = oneOf ":!#$%&*+./<=>?@\\^|-~_"
      , P.opLetter       = P.opStart sDef
      , P.reservedOpNames= ["=", "$", ";"]
      , P.reservedNames  = ["None"]
      , P.caseSensitive  = True
    }


lexer = P.makeTokenParser sDef


braces     = P.braces lexer
identifier = P.identifier lexer <|> P.operator lexer
reserved   = P.reserved   lexer
reservedOp = P.reservedOp lexer
parens     = P.parens     lexer
operator   = P.operator lexer
quotedParens p = char '\'' >> parens p

float      = SValNumber . SDouble <$> P.float      lexer
integer    = SValNumber . SInt <$> integer'
    where
        integer' = do
            f <- sign
            n <- natural
            return (f n)
        sign = (char '-' >> return negate)
            <|> return id

natural    = P.natural    lexer
semi       = P.semi       lexer
whiteSpace = P.whiteSpace lexer
