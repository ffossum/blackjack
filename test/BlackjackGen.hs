module BlackjackGen where

import Card
import Test.QuickCheck

newtype TestCard =
  TestCard Card
  deriving (Eq, Show)

instance Arbitrary TestCard where
  arbitrary = TestCard <$> elements allCards

newtype TestDeck =
  TestDeck Deck
  deriving (Eq, Show)

instance Arbitrary TestDeck where
  arbitrary = TestDeck . Deck <$> shuffle allCards
