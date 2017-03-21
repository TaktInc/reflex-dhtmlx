{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}

module Main (main) where

import qualified Data.Text               as T
import Reflex.Dom
import Reflex.Dom.DHTMLX.DateTime

main :: IO ()
main = mainWidget $ do
    el "h1" $ text "Date Widget Test"
    date <- dhtmlxDateTimePicker Nothing never
    dynText $ maybe "Nothing" (T.pack . show) <$> date
    return ()
