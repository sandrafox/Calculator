{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative 
import Control.Monad
import Data.Char (isDigit)
import Text.Parser.Combinators

data BinOperation = Plus | Minus | Mul | Div

data Expression = Brackets Expression 
               | BinExpression BinOperation Expression Expression 
               | Pi
               | E
               | Power Char Expression Expression
               | Number Int

main :: IO ()
main = do
    str <- getLine
    case runP parseE str of
        (Just (expr, "")) -> case evalExpression expr of
            (Left err) -> putStrLn err
            (Right i) -> putStrLn $ show i
        _ -> putStrLn "Wrong expression"


newtype Parser a = Parser { runP :: String -> Maybe (a, String) }

first :: (a -> c) -> (a, b) -> (c, b)
first f (r, s) = (f r, s)

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser parser) = Parser (fmap (first f) . parser)

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = Parser $ \s -> Just (a, s)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    Parser pf <*> Parser pa = Parser $ \s -> do
        (f, t) <- pf s
        (a, r) <- pa t
        pure (f a, r)

instance Alternative Parser where
    empty :: Parser a 
    empty = Parser $ \ _ -> Nothing

    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser f1) <|> (Parser f2) = Parser $ \s -> 
        let sa = f1 s 
        in case sa of
            Nothing -> f2 s
            a -> a

instance Monad Parser where
    return :: a -> Parser a
    return = pure

    (Parser p) >>= qf = Parser $ \s -> do
        (r, t) <- p s
        runP (qf r) t

ok :: Parser ()
ok = Parser $ \s -> Just ((), s)

isnot :: Parser a -> Parser ()
isnot parser = Parser $ \s -> case runP parser s of
    Just _  -> Nothing
    Nothing -> Just ((), s)

eof :: Parser ()
eof = Parser $ \s -> case s of
    [] -> Just ((), "")
    _  -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
    []     -> Nothing
    (x:xs) -> if p x then Just (x, xs) else Nothing

notok :: Parser ()
notok = isnot ok

char :: Char -> Parser Char
char c = satisfy (== c)

anyChar, digit :: Parser Char
anyChar = satisfy (const True)
digit   = satisfy isDigit

int :: Parser Expression
int = Number . read <$> (some digit)

parseE :: Parser Expression
parseE = chainl1 parseT parseAM

parseAM :: Parser (Expression -> Expression -> Expression)
parseAM = BinExpression <$> fmap toBinOp (char '+' <|> char '-')

parseMD :: Parser (Expression -> Expression -> Expression)
parseMD = BinExpression <$> fmap toBinOp (char '*' <|> char '/')

parsePow :: Parser (Expression -> Expression -> Expression)
parsePow = Power <$> char '^'

toBinOp :: Char -> BinOperation
toBinOp '+' = Plus
toBinOp '-' = Minus
toBinOp '*' = Mul
toBinOp '/' = Div

parseT :: Parser Expression
parseT = chainl1 parseP parseMD

parseP :: Parser Expression
parseP = chainl1 parseF parsePow

parseF :: Parser Expression
parseF = int <|> char '(' *> parseE <* char ')'

evalExpression :: Expression -> Either String Int
evalExpression (Number i) = Right i
evalExpression (BinExpression Plus e1 e2) = (+) <$> evalExpression e1 <*> evalExpression e2
evalExpression (BinExpression Minus e1 e2) = (-) <$> evalExpression e1 <*> evalExpression e2
evalExpression (BinExpression Mul e1 e2) = (*) <$> evalExpression e1 <*> evalExpression e2
evalExpression (BinExpression Div e1 e2) = let r2 = evalExpression e2 in
    case r2 of
        (Right 0) -> Left "Division by zero"
        r -> div <$> evalExpression e1 <*> r
evalExpression (Power _ e1 e2) = (^) <$> evalExpression e1 <*> evalExpression e2
