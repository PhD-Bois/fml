{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import qualified Data.Text as Text
import System.IO (hFlush, stdout)

import Parser (parseUnit)

main :: IO ()
main = forever $ do
    putStr "> "
    hFlush stdout
    code <- fmap Text.pack getLine
    print $ parseUnit "test" code
