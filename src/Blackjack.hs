module Blackjack
  ( someFunc
  ) where

import Card

data Outcome
  = DealerWin
  | PlayerWin

data GameState = GameState
  { deck :: [Card]
  , playerHand :: Hand
  , dealerHand :: Hand
  }

isBlackjack :: Hand -> Bool
isBlackjack hand = (handScore hand) == 21

anyBlackjack :: GameState -> Bool
anyBlackjack gs = isBlackjack (playerHand gs) || isBlackjack (dealerHand gs)

outcome :: GameState -> Maybe Outcome
outcome gs
  | playerScore > 21 = Just DealerWin
  | dealerScore > 21 = Just PlayerWin
  | playerScore == 21 && dealerScore == 21 = Just PlayerWin
  | playerScore == 22 && dealerScore == 22 = Just DealerWin
  | otherwise = Nothing
  where
    playerScore = handScore (playerHand gs)
    dealerScore = handScore (dealerHand gs)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
