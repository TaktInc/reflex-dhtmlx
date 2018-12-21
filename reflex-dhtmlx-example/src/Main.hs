{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}

module Main (main) where

import           Control.Monad.IO.Class     (liftIO)
import qualified Data.Text                  as T
import           Data.Time.Clock            (addUTCTime, getCurrentTime,
                                             utctDay)
import           Data.Time.LocalTime
import           Reflex.Dom
import           Reflex.Dom.DHTMLX.Date
import           Reflex.Dom.DHTMLX.DateTime


app :: MonadWidget t m => m ()
app = do
  zone <- liftIO getCurrentTimeZone

  yesterday <- addUTCTime (realToFrac . negate $ 60 * 60 * 24) <$> liftIO getCurrentTime

  el "section" $ do
    el "h2" $ text "Date Time Widget Test"
    el "div" $ text "we set this widget to be 24 hours ago by default"
    rec date <- dhtmlxDateTimePicker $ def
                  & dateTimePickerConfig_button .~ Just (_element_raw e)
                  & dateTimePickerConfig_timeZone .~ zone
                  & dateTimePickerConfig_initialValue .~ Just yesterday
        (e,_) <- el' "button" $ text "cal"
    el "div" . dynText $ T.pack . ("selected time is: " ++) . show <$> value date
    el "span" . text . T.pack $ "your time zone is: " ++ show zone
    return date

    el "h2" $ text "Date Time Widget based on the above widget"
    el "div" $ text "we leave this one unset to show default behavior"
    rec date' <- dhtmlxDateTimePicker $ def
                  & dateTimePickerConfig_button .~ Just (_element_raw e)
                  & dateTimePickerConfig_timeZone .~ zone
                  & dateTimePickerConfig_setValue .~ updated (_dateTimePicker_value date)
        (e,_) <- el' "button" $ text "cal"
    el "div" . dynText $ T.pack . ("selected time is: " ++) . show <$> value date'


  el "section" $ do
    el "h2" $ text "Date Widget Test"
    el "div" $ text "we set this widget to be 24 hours ago by default"
    rec date <- dhtmlxDatePicker $ def
                  & datePickerConfig_button .~ Just (_element_raw e)
                  & datePickerConfig_initialValue .~ Just (utctDay yesterday)
        (e,_) <- el' "button" $ text "cal"
    el "div" . dynText $ T.pack . ("selected time is: " ++) . show <$> value date

    el "h2" $ text "Date Widget Test based on the above widget"
    el "div" $ text "we leave this one unset to show default behavior"
    rec date' <- dhtmlxDatePicker $ def
                  & datePickerConfig_button .~ Just (_element_raw e)
                  & datePickerConfig_setValue .~ updated (_datePicker_value date)
        (e,_) <- el' "button" $ text "cal"
    el "div" . dynText $ T.pack . ("selected time is: " ++) . show <$> value date'


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
