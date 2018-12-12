{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}

module Main (main) where

import qualified Data.Text                         as T
import           Language.Javascript.JSaddle.Types (JSM)
import           Reflex.Dom
import           Reflex.Dom.DHTMLX.DateTime


app :: MonadWidget t m => m ()
app = do
  el "h1" $ text "Date Widget Test"
  rec date <- dhtmlxDateTimePicker $ def
                & dateTimePickerConfig_button .~ (Just $ _element_raw e)
      (e,_) <- el' "button" $ text "cal"
  el "div" $
    dynText $ maybe "Nothing" (T.pack . show) <$> value date
  return ()

{-|
   It's important that we do not use @mainWidget@ here.

   If you create the dhtmlx calendar on page load, like in this example,
   it needs to attach the calendar "base" to "document.body". If we
   use @mainWidget@ reflex-dom will clear out anything attached to the body
   at that time, including the "base". We attach the reflex-dom app
   to "#stage" to get around this problem.
-}
main :: IO ()
main = mainWidgetInElementById "stage" app
