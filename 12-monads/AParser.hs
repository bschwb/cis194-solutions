{- from Cis194 by Brent Yorgey
 - http://www.seas.upenn.edu/~cis194/spring13/extras/11-applicative2/AParser.hs
 -}
module AParser where

import Control.Applicative
import Data.Char
import Prelude hiding (sequence)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

inParser f = Parser . f . runParser

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor Parser where
  fmap = inParser . fmap . fmap . first

instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))
  (Parser fp) <*> xp = Parser $ \s ->
    case fp s of
      Nothing     -> Nothing
      Just (f,s') -> runParser (f <$> xp) s'

instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser p1 <|> Parser p2 = Parser $ liftA2 (<|>) p1 p2

instance Monad Parser where
  return = pure
  p >>= k = Parser f
    where f str = case runParser p str of
                    Nothing -> Nothing
                    Just (a, resStr) -> runParser (k a) resStr

parseFile :: Parser [[Int]]
parseFile = many parseLine

parseLine :: Parser [Int]
parseLine = parseInt' >>= \i -> replicateM i parseInt'

parseInt' :: Parser Int
parseInt' = spaces *> parseInt

parseInt :: Parser Int
parseInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (ma:mas) =
  ma >>= \a ->
  sequence mas >>= \as ->
  return (a:as)

replicateM :: Monad m => Int -> m a -> m [a]
replicateM n m = sequence (replicate n m)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

