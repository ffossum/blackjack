{-# LANGUAGE OverloadedStrings #-}

import Blackjack
import BlackjackGen
import Card
import Test.Hspec
import Test.QuickCheck

gameHadBlackjack :: GameSummary -> Bool
gameHadBlackjack = anyBlackjack . gameSummaryGameState

main :: IO ()
main =
  hspec $ do
    describe "hand score" $ do
      describe "ace" $
        it "has a score of 11" $ handScore [Card Hearts Ace] `shouldBe` 11
      describe "face card" $
        it "has a score of 10" $ do
          handScore [Card Spades Jack] `shouldBe` 10
          handScore [Card Diamonds Queen] `shouldBe` 10
          handScore [Card Clubs King] `shouldBe` 10
      describe "numbered card" $
        it "has a score equal to its value" $ do
          handScore [Card Spades (Numbered 2)] `shouldBe` 2
          handScore [Card Clubs (Numbered 5)] `shouldBe` 5
          handScore [Card Diamonds (Numbered 8)] `shouldBe` 8
          handScore [Card Hearts (Numbered 10)] `shouldBe` 10
    describe "game" $ do
      it "alternately deals 2 cards to player and dealer, starting with player" $ do
        let deck =
              Deck
                [ Card Clubs Ace
                , Card Diamonds (Numbered 5)
                , Card Hearts (Numbered 9)
                , Card Hearts Queen
                ]
        let Right summary = runGame (PlayerName "sam") deck
        let (GameState deck playerHand dealerHand) =
              gameSummaryGameState summary
        deck `shouldBe` Deck []
        playerHand `shouldBe` [Card Hearts (Numbered 9), Card Clubs Ace]
        dealerHand `shouldBe` [Card Hearts Queen, Card Diamonds (Numbered 5)]
      it "if player has blackjack no more cards are dealt, and player wins" $ do
        let deck =
              Deck
                [ Card Clubs Ace
                , Card Diamonds (Numbered 2)
                , Card Hearts (Numbered 10)
                , Card Hearts Queen
                , Card Spades (Numbered 3)
                ]
        let Right summary = runGame (PlayerName "sam") deck
        let (GameState deck playerHand dealerHand) =
              gameSummaryGameState summary
        length playerHand `shouldBe` 2
        length dealerHand `shouldBe` 2
        (gameSummaryOutcome summary) `shouldBe` PlayerWin
      it
        "if dealer has blackjack and player does not, no more cards are dealt and dealer wins" $ do
        let deck =
              Deck
                [ Card Diamonds (Numbered 2)
                , Card Hearts (Numbered 10)
                , Card Hearts Queen
                , Card Clubs Ace
                , Card Spades (Numbered 3)
                ]
        let Right summary = runGame (PlayerName "sam") deck
        let (GameState deck playerHand dealerHand) =
              gameSummaryGameState summary
        length playerHand `shouldBe` 2
        length dealerHand `shouldBe` 2
        (gameSummaryOutcome summary) `shouldBe` DealerWin
      context "when neither player nor dealer has blackjack" $ do
        it "player draws cards until he reaches 17 or higher" $
          property
            (\(TestDeck deck) -> do
               let Right summary = runGame (PlayerName "sam") deck
               let playerHand =
                     (gameStatePlayerHand . gameSummaryGameState) summary
               not (gameHadBlackjack summary) ==> handScore playerHand `shouldSatisfy`
                 (>= 17))
        it "player loses if his total reaches higher than 21" $
          property
            (\(TestDeck deck) -> do
               let Right summary = runGame (PlayerName "sam") deck
                   (GameState _ playerHand _) = gameSummaryGameState summary
                   playerScore = handScore playerHand
               playerScore >
                 21 ==> gameSummaryOutcome summary `shouldBe` DealerWin)
        it "dealer draws until their score is higher than the player" $
          property
            (\(TestDeck deck) -> do
               let Right summary = runGame (PlayerName "sam") deck
                   (GameState _ playerHand dealerHand) =
                     gameSummaryGameState summary
                   playerScore = handScore playerHand
                   dealerScore = handScore dealerHand
               not (gameHadBlackjack summary) &&
                 playerScore <
                 21 ==> dealerScore `shouldSatisfy` (>= playerScore))
        it "dealer loses if his total reaches higher than 21" $
          property
            (\(TestDeck deck) -> do
               let Right summary = runGame (PlayerName "sam") deck
                   (GameState _ _ dealerHand) = gameSummaryGameState summary
                   dealerScore = handScore dealerHand
               dealerScore >
                 21 ==> gameSummaryOutcome summary `shouldBe` PlayerWin)
