module Blackjack
  ( someFunc
  ) where

import Card

data Outcome
  = DealerWin
  | PlayerWin
  | Push

someFunc :: IO ()
someFunc = putStrLn "someFunc"
