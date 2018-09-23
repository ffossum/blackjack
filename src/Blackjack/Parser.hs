{-# LANGUAGE OverloadedStrings #-}

module Blackjack.Parser
  ( parseDeck
  ) where

import Card
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as P

import Data.Functor
import Data.Text

parseDeck :: Text -> Either String Deck
parseDeck = parseOnly deck

deck :: Parser Deck
deck = do
  d <- Deck <$> cards
  _ <- option () separator
  _ <- endOfInput
  pure d
  where
    cards = card `sepBy1'` separator

card :: Parser Card
card = do
  s <- suit
  v <- value
  pure $ Card s v

value :: Parser Value
value = choice [n, j, q, k, a]
  where
    n = Numbered <$> decimal
    j = Jack <$ char 'J'
    q = Queen <$ char 'Q'
    k = King <$ char 'K'
    a = Ace <$ char 'A'

suit :: Parser Suit
suit = choice [c, d, h, s]
  where
    c = Clubs <$ char 'C'
    d = Diamonds <$ char 'D'
    h = Hearts <$ char 'H'
    s = Spades <$ char 'S'

separator :: Parser ()
separator = do
  _ <- char ','
  _ <- many' (char ' ')
  pure ()
