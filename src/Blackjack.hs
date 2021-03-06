{-# LANGUAGE OverloadedStrings #-}

module Blackjack
  ( runGame
  , randomDeck
  , GameSummary(..)
  , GameState(..)
  , gameSummaryAsText
  , Outcome(..)
  , PlayerName(..)
  , Deck(..)
  , anyBlackjack
  ) where

import Card
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T
import System.Random.Shuffle

newtype PlayerName =
  PlayerName Text
  deriving (Eq, Show)

data Outcome
  = DealerWin
  | PlayerWin
  deriving (Eq, Show)

data GameState = GameState
  { gameStateDeck :: Deck
  , gameStatePlayerHand :: Hand
  , gameStateDealerHand :: Hand
  } deriving (Eq, Show)

data GameSummary = GameSummary
  { gameSummaryPlayerName :: PlayerName
  , gameSummaryGameState :: GameState
  , gameSummaryOutcome :: Outcome
  } deriving (Eq, Show)

randomDeck :: IO Deck
randomDeck = Deck <$> shuffleM allCards

runGame :: PlayerName -> Deck -> Either String GameSummary
runGame playerName deck = evalState f <$> newGameState deck
  where
    f = do
      initialState <- get
      finalState <-
        if (anyBlackjack initialState)
          then pure initialState
          else do
            modify' playerPhase
            modify' dealerPhase
            get
      let outcome = determineOutcome finalState
      return (GameSummary playerName finalState outcome)

newGameState :: Deck -> Either String GameState
newGameState (Deck (c0:c1:c2:c3:cs)) =
  Right $ GameState (Deck cs) [c2, c0] [c3, c1]
newGameState _ = Left "Not enough cards in the deck."

playerPhase :: GameState -> GameState
playerPhase gs =
  if playerShouldDraw gs
    then maybe gs playerPhase (drawCardForPlayer gs)
    else gs

dealerPhase :: GameState -> GameState
dealerPhase gs =
  if dealerShouldDraw gs
    then maybe gs dealerPhase (drawCardForDealer gs)
    else gs

getPlayerScore :: GameState -> Int
getPlayerScore = handScore . gameStatePlayerHand

getDealerScore :: GameState -> Int
getDealerScore = handScore . gameStateDealerHand

playerShouldDraw :: GameState -> Bool
playerShouldDraw gs = (getPlayerScore gs) < 17 && (not dealerHasBlackjack)
  where
    dealerHasBlackjack = isBlackjack (gameStateDealerHand gs)

anyBlackjack :: GameState -> Bool
anyBlackjack (GameState deck ph dh) = isBlackjack ph || isBlackjack dh

isBlackjack :: Hand -> Bool
isBlackjack h@[_, _] = handScore h == 21
isBlackjack _ = False

dealerShouldDraw :: GameState -> Bool
dealerShouldDraw gs =
  let playerScore = getPlayerScore gs
      dealerScore = getDealerScore gs
      -- in real blackjack the dealer should draw if playerScore <=21,
      -- but in this game <21 is fine, since the player wins 21 vs 21.
  in (playerScore < 21) && (dealerScore < playerScore)

peekTopCard :: GameState -> Maybe Card
peekTopCard (GameState (Deck (c:_)) _ _) = Just c
peekTopCard _ = Nothing

removeTopCard :: GameState -> GameState
removeTopCard (GameState (Deck (_:cs)) ph dh) = GameState (Deck cs) ph dh

drawCardForPlayer :: GameState -> Maybe GameState
drawCardForPlayer gs = do
  card <- peekTopCard gs
  pure $ (addCardToPlayerHand card . removeTopCard) gs

drawCardForDealer :: GameState -> Maybe GameState
drawCardForDealer gs = do
  card <- peekTopCard gs
  pure $ (addCardToDealerHand card . removeTopCard) gs

addCardToHand :: Card -> Hand -> Hand
addCardToHand = (:)

addCardToPlayerHand :: Card -> GameState -> GameState
addCardToPlayerHand c (GameState d ph dh) = GameState d (addCardToHand c ph) dh

addCardToDealerHand :: Card -> GameState -> GameState
addCardToDealerHand c (GameState d ph dh) = GameState d ph (addCardToHand c dh)

determineOutcome :: GameState -> Outcome
determineOutcome gs
  | playerScore > 21 = DealerWin
  | dealerScore > 21 = PlayerWin
  | playerScore == 21 && dealerScore == 21 = PlayerWin
  | playerScore == 22 && dealerScore == 22 = DealerWin
  | otherwise
    -- this case should never happen, but adding it anyway to make function total
   =
    if playerScore > dealerScore
      then PlayerWin
      else DealerWin
  where
    playerScore = getPlayerScore gs
    dealerScore = getDealerScore gs

gameSummaryAsText :: GameSummary -> [Text]
gameSummaryAsText (GameSummary (PlayerName playerName) gs outcome) =
  [winner, playerCards, dealerCards]
  where
    winner =
      case outcome of
        PlayerWin -> playerName
        DealerWin -> "dealer"
    playerCards =
      let h = gameStatePlayerHand gs
      in playerName <> ": " <> (T.intercalate ", " (cardAsText <$> reverse h))
    dealerCards =
      let h = gameStateDealerHand gs
      in "dealer: " <> (T.intercalate ", " (cardAsText <$> reverse h))
