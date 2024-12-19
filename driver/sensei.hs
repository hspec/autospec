module Main (main) where

import           System.Environment

import qualified XMonad
import           Run

main :: IO ()
main = do
  XMonad.tagSelfWith "sensei"
  getArgs >>= run
