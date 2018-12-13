{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}

module Main (main) where

import qualified Data.Text                         as T
import           Data.Time.LocalTime
import           Language.Javascript.JSaddle.Types (JSM, liftJSM)
import           Reflex.Dom
import           Reflex.Dom.DHTMLX.Date
import           Reflex.Dom.DHTMLX.DateTime


app :: MonadWidget t m => m ()
app = do
  zone <- liftJSM getCurrentTimeZone

  el "section" $ do
    el "h2" $ text "Date Time Widget Test"
    rec date <- dhtmlxDateTimePicker $ def
                  & dateTimePickerConfig_button .~ Just (_element_raw e)
                  & dateTimePickerConfig_timeZone .~ zone
        (e,_) <- el' "button" $ text "cal"
    el "div" . dynText $ T.pack . ("selected time is: " ++) . show <$> value date
    el "span" . text . T.pack $ "your time zone is: " ++ show zone

  el "section" $ do
    el "h2" $ text "Date Widget Test"
    rec date <- dhtmlxDatePicker $ def
                  & datePickerConfig_button .~ Just (_element_raw e)
        (e,_) <- el' "button" $ text "cal"
    el "div" . dynText $ T.pack . ("selected time is: " ++) . show <$> value date


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
