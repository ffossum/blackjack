module Blackjack
  ( someFunc
  ) where

import Card

data Outcome
  = DealerWin
  | PlayerWin
  | Push

data GameState = GameState
  { deck :: [Card]
  , playerHand :: Hand
  , dealerHand :: Hand
  }

isBlackjack :: Hand -> Bool
isBlackjack hand = (handScore hand) == 21

anyBlackjack :: GameState -> Bool
anyBlackjack gs = isBlackjack (playerHand gs) || isBlackjack (dealerHand gs)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
