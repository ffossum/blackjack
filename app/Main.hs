{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blackjack
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

playerName :: PlayerName
playerName = PlayerName "sam"

main :: IO ()
main = do
  deck <- randomDeck
  let summary = runGame playerName deck
  TIO.putStr (gameSummaryAsText summary)
