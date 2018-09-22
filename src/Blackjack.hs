module Blackjack
  ( someFunc
  ) where

import Card
import Control.Monad.State

newtype Deck =
  Deck [Card]

data Outcome
  = DealerWin
  | PlayerWin

data GameState = GameState
  { gameStateDeck :: Deck
  , gameStatePlayerHand :: Hand
  , gameStateDealerHand :: Hand
  }

newGameState :: Deck -> GameState
newGameState (Deck (c0:c1:c2:c3:cs)) =
  GameState (Deck cs) (Hand [c0, c1]) (Hand [c2, c3])
newGameState _ = undefined -- TODO

drawCardForPlayer :: GameState -> GameState
drawCardForPlayer gs =
  let (c, gs') = drawCard gs
  in addCardToPlayerHand c gs'

drawCardForDealer :: GameState -> GameState
drawCardForDealer gs =
  let (c, gs') = drawCard gs
  in addCardToDealerHand c gs'

drawCard :: GameState -> (Card, GameState)
drawCard (GameState (Deck (c:cs)) ph dh) = (c, nextState)
  where
    nextState = (GameState (Deck cs) ph dh)

addCardToHand :: Card -> Hand -> Hand
addCardToHand c (Hand cs) = Hand (c : cs)

addCardToPlayerHand :: Card -> GameState -> GameState
addCardToPlayerHand c (GameState d ph dh) =
  GameState d ((addCardToHand c) ph) dh

addCardToDealerHand :: Card -> GameState -> GameState
addCardToDealerHand c (GameState d ph dh) =
  GameState d ph ((addCardToHand c) dh)

anyBlackjack :: GameState -> Bool
anyBlackjack gs =
  isBlackjack (gameStatePlayerHand gs) || isBlackjack (gameStateDealerHand gs)

isBlackjack :: Hand -> Bool
isBlackjack hand = (handScore hand) == 21

outcome :: GameState -> Maybe Outcome
outcome gs
  | playerScore > 21 = Just DealerWin
  | dealerScore > 21 = Just PlayerWin
  | playerScore == 21 && dealerScore == 21 = Just PlayerWin
  | playerScore == 22 && dealerScore == 22 = Just DealerWin
  | otherwise = Nothing
  where
    playerScore = handScore (gameStatePlayerHand gs)
    dealerScore = handScore (gameStateDealerHand gs)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
