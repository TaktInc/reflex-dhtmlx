{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}

module Main (main) where

import Reflex.Dom
import Reflex.Dom.DHTMLX

main :: IO ()
main = mainWidget $ do
    el "h1" $ text "Date Widget Test"
    dhtmlxDatePicker Nothing never
