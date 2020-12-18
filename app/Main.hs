{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import qualified Data.Text as Text
import System.IO (hFlush, stdout)

import Text.Megaparsec

import Parser (parseUnit)

main :: IO ()
main = forever $ do
    putStr "> "
    hFlush stdout
    code <- fmap Text.pack getLine
    case parseUnit "test" code of
        Left bundle -> putStrLn (errorBundlePretty bundle)
        Right ast -> print ast
