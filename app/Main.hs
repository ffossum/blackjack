{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blackjack
import Blackjack.Parser
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment

playerName :: PlayerName
playerName = PlayerName "sam"

getDeck :: [String] -> IO (Either String Deck)
getDeck args =
  case args of
    [path] -> do
      deckString <- TIO.readFile path
      pure $ parseDeck deckString
    [] -> Right <$> randomDeck
    _ -> pure $ Left "Invalid arguments"

main :: IO ()
main = do
  args <- getArgs
  d <- getDeck args
  case d of
    Right deck -> do
      let summary = runGame playerName deck
      case summary of
        Right s -> printSummary s
        Left err -> putStrLn err
    Left e -> putStrLn e

printSummary :: GameSummary -> IO ()
printSummary = traverse_ TIO.putStrLn . gameSummaryAsText
