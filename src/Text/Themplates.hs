{-# LANGUAGE PatternGuards, ScopedTypeVariables, TupleSections #-}

module Text.Themplates
  ( Chunk
  , parseSplices, substSplices

  -- * Parsec parsers
  , spliceParser, curlySplice, thSplice, nestParser, escapeParser, delimParser

  -- * Misc Utilities
  , generateNames
  ) where

import Control.Applicative        ( (<$>), (<*>) )
import Control.Monad.Trans.Either ( EitherT )
import Data.Generics              ( Data, extT, everywhere )
import Data.List                  ( isPrefixOf, tails )
import qualified Data.Map as M
import Data.Maybe                 ( maybeToList )
import Text.Parsec
  ( Parsec, parse, try, eof, anyToken, noneOf, char, string, choice, (<|>)
  , lookAhead, anyChar, manyTill )

type Chunk c s = Either c s

substSplices :: forall t s e m r. (Monad m, Data r, Ord r)
             => (s -> [t])
             -> ([t] -> EitherT e m r)
             -> (s -> EitherT e m [(r, r)])
             -> [Chunk [t] s]
             -> EitherT e m r
substSplices placeholder parser subst xs = do
  subs <- sequence [subst s | Right s <- xs]

  let subs_map = M.fromList $ concat subs
      do_subst :: r -> r
      do_subst x
        | Just r <- M.lookup x subs_map = r
        | otherwise                     = x

  parsed <- parser
          $ concatMap (either id placeholder) xs

  return $ everywhere (id `extT` do_subst) parsed

-- Utilities for parsing spliced stuff.

parseSplices :: forall t s. Show t
             => Parsec [t] () s
             -> [t]
             -> Either String [Chunk [t] s]
parseSplices splice =
  either (Left . show) Right . parse (spliceParser splice) ""

spliceParser :: forall t s. Show t
             => Parsec [t] () s
             -> Parsec [t] () [Chunk [t] s]
spliceParser parse_splice = do
  s <-  (Right         <$> try parse_splice)
    <|> (Left  . (:[]) <$> anyToken)
    <|> (eof >> return (Left  []))
  case s of
    c@(Left  []) -> return [c]
    _ -> do
      rest <- spliceParser parse_splice
      case (s, rest) of
        (Left  [c], Left  acc:rs) -> return $ Left  (c:acc) : rs
        _ -> return $ s : rest

-- The same splice style as the old ast quoters.
-- TODO: Make the quasi-quoter configurable to use this.
thSplice :: Parsec String () (Maybe String, String)
thSplice = do
  _ <- try $ string "$("
  fancySplice (concat <$> nestParser (delimParser '(' ')')
                                     [try $ char ')' >> return ""])

-- To be passed as the first parameter to parseSplices or spliceParser.
curlySplice :: Parsec String () (Maybe String, String)
curlySplice = do
  _ <- try $ string "{{"
  fancySplice (concat <$> nestParser (delimParser '{' '}')
                                     [try $ string "}}" >> return ""])

fancySplice :: Parsec String () s
            -> Parsec String () (Maybe String, s)
fancySplice code_parser = do
  c <- lookAhead anyChar
  case c of
    '<' -> do
      _ <- char '<'
      splice <- manyTill
        (escapeParser '\\' [('>', '>'), ('\\', '\\')])
        (char '>')
      code <- code_parser
      return (Just splice, code)
    _ ->  do
      code <- code_parser
      return (Nothing, code)

{-
parseList :: Parsec String () (Either String (String, String, String))
parseList = do
  input <- getInput
  (try $ do
    prefix <- manyTill anyChar (lookAhead ((string "..." >> return ())
                                         <|> eof))
    string "..."
    rest <- many (noneOf " ")
    return $ Right (prefix, rest)
    ) <|> (many anyChar >> return (Left input))
-}

nestParser
  :: forall t r. Show t
  =>  Parsec [t] () (r, Maybe (Parsec [t] () r))
  -> [Parsec [t] () r]
  ->  Parsec [t] () [r]
nestParser open closers = case closers of
  [] -> return []
  (close:cs)
    -> ((:) <$> close <*> nestParser open cs)
   <|> (open >>= \(x, c) -> (x:) <$> nestParser open (maybeToList c ++ closers))
   <|> return []

escapeParser :: Char -> [(Char, Char)] -> Parsec String () Char
escapeParser c xs =
    (char c >> choice (map escape xs)) <|> noneOf [c]
  where
    escape (code, repl) = char code >> return repl

delimParser :: Char -> Char
            -> Parsec String () (String, Maybe (Parsec String () String))
delimParser start end = do
  r <- try (string [start]) <|> ((:[]) <$> noneOf [end])
  return (r, if r == [start] then Just (try $ string [end]) else Nothing)

generateNames :: String -> String -> [String]
generateNames prefix input =
    [ prefix ++ s
    | s <- map show [(0::Int)..]
    , all (not . isPrefixOf s) avoids
    ]
  where
    avoids = [ drop (length prefix) t
             | t <- tails input
             , prefix `isPrefixOf` t
             ]
