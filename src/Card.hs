{-# LANGUAGE OverloadedStrings #-}

module Card where

import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import TextShow

data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving (Eq, Show)

data Value
  = Numbered Int
  | Jack
  | Queen
  | King
  | Ace
  deriving (Eq, Show)

data Card =
  Card Suit
       Value
  deriving (Eq, Show)

newtype Deck =
  Deck [Card]
  deriving (Show)

newtype Hand =
  Hand [Card]
  deriving (Eq, Show)

suitAsText :: Suit -> Text
suitAsText Clubs = "C"
suitAsText Diamonds = "D"
suitAsText Hearts = "H"
suitAsText Spades = "S"

valueAsText :: Value -> Text
valueAsText (Numbered i) = showt i
valueAsText Jack = "J"
valueAsText Queen = "Q"
valueAsText King = "K"
valueAsText Ace = "A"

cardAsText :: Card -> Text
cardAsText (Card suit value) = (suitAsText suit) <> (valueAsText value)

allSuits :: [Suit]
allSuits = [Clubs, Diamonds, Hearts, Spades]

allValues :: [Value]
allValues = (Numbered <$> [2 .. 10]) <> [Jack, Queen, King, Ace]

allCards :: [Card]
allCards = do
  s <- allSuits
  v <- allValues
  pure $ Card s v

cardScore :: Card -> Int
cardScore (Card _ value) =
  case value of
    (Numbered v) -> v
    Ace -> 11
    _ -> 10

handScore :: Hand -> Int
handScore (Hand hand) = foldl' addCard 0 hand
  where
    addCard score card = score + (cardScore card)
