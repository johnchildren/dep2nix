{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as T

import           Lib

main :: IO ()
main = do
  depFile <- T.readFile "Gopkg.lock"
  case dep2nix depFile of
    Right expr -> T.putStrLn expr
    Left err   -> T.putStrLn "error"
