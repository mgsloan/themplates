{-# LANGUAGE PatternGuards, ScopedTypeVariables, TupleSections #-}

module Text.Themplates where

import Control.Applicative        ( (<$>), (<*>) )
import Control.Monad.Trans.Either ( EitherT )
import Data.Data                  ( Data )
import Data.Generics.Aliases      ( extT )
import Data.Generics.Schemes      ( everywhere )
import qualified Data.Map as M
import Data.Maybe                 ( maybeToList )
import Text.Parsec
  ( Parsec, try, eof, anyToken, noneOf, char, string, choice, (<|>) )

data Chunk t s = Chunk [t] | Splice s
  deriving Show

chunk :: ([t] -> a) -> (s -> a) -> Chunk t s -> a
chunk f _ (Chunk ts) = f ts
chunk _ g (Splice s) = g s

substSplices :: forall s t e m r. (Monad m, Data r, Ord r)
             => (s -> [t])
             -> ([t] -> EitherT e m r)
             -> (s   -> EitherT e m [(r, r)])
             -> [Chunk t s]
             -> EitherT e m r
substSplices placeholder parser subst xs = do
  subs <- sequence [subst s | Splice s <- xs]

  let subs_map = M.fromList $ concat subs
      do_subst :: r -> r
      do_subst x
        | Just r <- M.lookup x subs_map = r
        | otherwise                     = x

  parsed <- parser
          $ concatMap (chunk id placeholder) xs

  return $ everywhere (id `extT` do_subst) parsed


-- Utilities for parsing spliced stuff.

spliceParser :: forall t s. Show t
             => Parsec [t] () s
             -> Parsec [t] () [Chunk t s]
spliceParser parse_splice = do
  s <-  (Splice        <$> parse_splice)
    <|> (Chunk . (:[]) <$> anyToken)
    <|> (eof >> return (Chunk []))
  case s of
    c@(Chunk []) -> return [c]
    _ -> do
      rest <- spliceParser parse_splice
      case (s, rest) of
        (Chunk [c], Chunk acc:rs) -> return $ Chunk (c:acc) : rs
        _ -> return $ s : rest


nestParser :: forall t r. Show t
           =>  Parsec [t] () (r, Maybe (Parsec [t] () r))
           -> [Parsec [t] () r]
           ->  Parsec [t] () [r]
nestParser open closers = case closers of 
  [] -> return []
  (close:cs) -> ((:) <$> close <*> nestParser open cs) 
            <|> (open >>= \(x, c) -> (x:) <$> nestParser open (maybeToList c ++ closers))
            <|> return []


escapeParser :: Char -> [(Char, Char)] -> Parsec String () Char
escapeParser c xs
  = (char c >> choice (map escape xs)) <|> noneOf [c]
 where
  escape (code, repl) = char code >> return repl


delimParser :: Char -> Char
            -> Parsec String () (String, Maybe (Parsec String () String))
delimParser start end = do
  r <- try (string [start]) <|> ((:[]) <$> noneOf [end])
  return (r, if r == [start] then Just (try $ string [end]) else Nothing)