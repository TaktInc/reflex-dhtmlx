{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}

module Main (main) where

import qualified Data.Text                        as T
import           Language.Javascript.JSaddle.Types (JSM)
import           Reflex.Dom
import           Reflex.Dom.DHTMLX.DateTime


app :: MonadWidget t m => m ()
app = do
  el "h1" $ text "Date Widget Test"
  rec date <- dhtmlxDateTimePicker $ def
                & dateTimePickerConfig_button .~ (Just $ _element_raw e)
      (e,_) <- el' "span" $ text "cal"
  el "div" $
    dynText $ maybe "Nothing" (T.pack . show) <$> value date
  return ()


main :: IO ()
main = mainWidget app
