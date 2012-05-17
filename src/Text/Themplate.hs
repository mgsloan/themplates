{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}

module Text.Themplate where

import Control.Applicative   ( (<$>) )
import Data.Data             ( Data )
import Data.Either           ( rights )
import Data.Generics.Aliases ( extT )
import Data.Generics.Schemes ( everywhere )
import qualified Data.Map as M
import Text.Parsec
  ( Parsec, lookAhead, manyTill, eof, anyToken, parse, (<|>) )

type Chunk t s = Either [t] s

mapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapEither f g = either (Left . f) (Right . g)


substSplices :: forall s t e r. (Data r, Ord r)
             => (s -> [t])
             -> ([t] -> Either e r)
             -> (s   -> Either e (r, r))
             -> [Chunk t s]
             -> Either e r
substSplices placeholder parser subst xs = do
  subs <- mapM subst $ rights xs

  let subs_map = M.fromList subs
      do_subst :: r -> r
      do_subst x
        | Just r <- M.lookup x subs_map = r
        | otherwise                     = x

  parsed <- parser
          $ concatMap (either id placeholder) xs

  return $ everywhere (id `extT` do_subst) parsed

-- TODO: [t] --> t  ??

parseSplices :: Show t
             => Parsec [t] () [t]
             -> Parsec [t] () [t]
             -> Parsec [t] () [t]
             -> Parsec [t] () s
             -> [t] -> Either String [Chunk t s]
parseSplices opener_splice opener_other closer parse_splice
  = mapEither show consolidate . parse parser ""
 where
  consolidate [] = []
  consolidate (Left []         : xs) = consolidate xs
  consolidate (Left x : Left y : xs) = consolidate $ Left (x ++ y) : xs
  consolidate (x:xs)                 = x : consolidate xs

  anything_special 
    = eof <|> ((opener_splice <|> opener_other <|> closer) >> return ())

  -- This is probably awful parsec style.
  parser = do
    prefix <- manyTill anyToken $ lookAhead anything_special
    (lookAhead eof >> return [Left prefix])
      <|> (consume   opener_splice >>  ((:[]) . Right) <$> parse_splice)
      <|> (consume   opener_other  >>= parse_other)
      <|> (lookAhead closer        >> return [])

  consume x = lookAhead x >> x

  parse_other open = do
    nested <- parser
    close <- closer
    (([Left open] ++ nested ++ [Left close]) ++) <$> parser

--TODO: Something that gives / parses a splice type that refers to other quasiquotes

{-
              c <- lookAhead anyChar
              case c of

                '<' -> do
                  char '<'
                  splice <- fancy_escaped
                  char '>'
                  code <- splice_code 0
                  continue [ChunkSplice $ FancySplice splice code]

                _ ->  do
                  code <- splice_code 0
                  continue [ChunkSplice $ PlainSplice code]

            _ -> (ChunkLit "$":) <$> parser

        ')' -> return []

        _   -> fail "Impossible!"


  fancy_escaped = (string "\\>" >> fancy_escaped >>= return . ('>':))
              <|> ((:) <$> noneOf ">" <*> fancy_escaped)
              <|> return []

  parens p = (++")") . ('(':) <$> between (char '(') (char ')') p

  splice_code n = do
    a <- anyChar
    case a of
      '(' ->                               (a:) <$> splice_code (n + 1)
      ')' -> if n == 0 then return "" else (a:) <$> splice_code (n - 1)
      _   ->                               (a:) <$> splice_code n
-}