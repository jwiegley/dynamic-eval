{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lambda
import Reflex
import Reflex.Dom

main :: IO ()
main = mainWidget $ do
  el "h1" $ text "Input"
  exp <- el "div" $ editLamD id
  el "h1" $ text "Raw"
  viewLamD 0 text exp
  el "h1" $ text "Reduced"
  viewLamD 0 text $ whnfD exp
  pure ()
