module Card where

import Data.List

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

data Card = Card
  { cardSuit :: Suit
  , cardValue :: Value
  } deriving (Eq, Show)

newtype Hand =
  Hand [Card]
  deriving (Eq, Show)

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
cardScore c =
  case (cardValue c) of
    (Numbered v) -> v
    Ace -> 11
    _ -> 10

handScore :: Hand -> Int
handScore (Hand hand) = foldl' addCard 0 hand
  where
    addCard score card = score + (cardScore card)
