{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser

main :: IO ()
main = print $ parseUnit ""
