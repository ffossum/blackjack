{-# LANGUAGE OverloadedStrings #-}

module Blackjack
  ( runGame
  , randomDeck
  , gameSummaryAsText
  , PlayerName(..)
  ) where

import Card
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T
import System.Random.Shuffle

newtype PlayerName =
  PlayerName Text
  deriving (Show)

data Outcome
  = DealerWin
  | PlayerWin
  deriving (Show)

data GameState = GameState
  { gameStateDeck :: Deck
  , gameStatePlayerHand :: Hand
  , gameStateDealerHand :: Hand
  } deriving (Show)

data GameSummary = GameSummary
  { gameSummaryPlayerName :: PlayerName
  , gameSummaryGameState :: GameState
  , gameSummaryOutcome :: Outcome
  } deriving (Show)

randomDeck :: IO Deck
randomDeck = Deck <$> (shuffleM allCards)

runGame :: PlayerName -> Deck -> GameSummary
runGame playerName deck = evalState f initialState
  where
    initialState = newGameState deck
    f = do
      modify' drawPlayerCards
      modify' drawDealerCards
      finalState <- get
      let outcome = determineOutcome finalState
      return (GameSummary playerName finalState outcome)

newGameState :: Deck -> GameState
newGameState (Deck (c0:c1:c2:c3:cs)) =
  GameState (Deck cs) (Hand [c2, c0]) (Hand [c3, c1])
newGameState _ = undefined -- TODO

drawPlayerCards :: GameState -> GameState
drawPlayerCards gs =
  if playerShouldDraw gs
    then drawPlayerCards (drawCardForPlayer gs)
    else gs

drawDealerCards :: GameState -> GameState
drawDealerCards gs =
  if dealerShouldDraw gs
    then drawDealerCards (drawCardForDealer gs)
    else gs

getPlayerScore :: GameState -> Int
getPlayerScore = handScore . gameStatePlayerHand

getDealerScore :: GameState -> Int
getDealerScore = handScore . gameStateDealerHand

playerShouldDraw :: GameState -> Bool
playerShouldDraw gs = (getPlayerScore gs) < 17

dealerShouldDraw :: GameState -> Bool
dealerShouldDraw gs =
  let playerScore = getPlayerScore gs
      dealerScore = getDealerScore gs
  in (playerScore <= 21) && (dealerScore < playerScore)

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
    nextState = GameState (Deck cs) ph dh

addCardToHand :: Card -> Hand -> Hand
addCardToHand c (Hand cs) = Hand (c : cs)

addCardToPlayerHand :: Card -> GameState -> GameState
addCardToPlayerHand c (GameState d ph dh) =
  GameState d ((addCardToHand c) ph) dh

addCardToDealerHand :: Card -> GameState -> GameState
addCardToDealerHand c (GameState d ph dh) = GameState d ph (addCardToHand c dh)

determineOutcome :: GameState -> Outcome
determineOutcome gs
  | playerScore > 21 = DealerWin
  | dealerScore > 21 = PlayerWin
  | playerScore == 21 && dealerScore == 21 = PlayerWin
  | playerScore == 22 && dealerScore == 22 = DealerWin
  | otherwise =
    if playerScore > dealerScore
      then PlayerWin
      else DealerWin
  where
    playerScore = getPlayerScore gs
    dealerScore = getDealerScore gs

gameSummaryAsText :: GameSummary -> Text
gameSummaryAsText (GameSummary (PlayerName playerName) gs outcome) =
  winner <> "\n" <> playerCards <> "\n" <> dealerCards <> "\n"
  where
    winner =
      case outcome of
        PlayerWin -> playerName
        DealerWin -> "dealer"
    playerCards =
      let (Hand cs) = gameStatePlayerHand gs
      in playerName <> ": " <> (T.intercalate ", " (cardAsText <$> reverse cs))
    dealerCards =
      let (Hand cs) = gameStateDealerHand gs
      in "dealer: " <> (T.intercalate ", " (cardAsText <$> reverse cs))
