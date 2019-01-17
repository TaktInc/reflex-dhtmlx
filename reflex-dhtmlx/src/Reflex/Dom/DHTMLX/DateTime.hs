{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Reflex.Dom.DHTMLX.DateTime
  ( dhtmlxDateTimePicker
  , DateTimePickerConfig (..)
  , DateTimePicker
  , _dateTimePicker_value
  , dateTimePickerConfig_initialValue
  , dateTimePickerConfig_setValue
  , dateTimePickerConfig_button
  , dateTimePickerConfig_parent
  , dateTimePickerConfig_weekStart
  , dateTimePickerConfig_minutesInterval
  , dateTimePickerConfig_attributes
  , dateTimePickerConfig_visibleOnLoad
  , dateTimePickerConfig_timeZone
  , dateTimePickerConfig_hideRule
  , MinutesInterval (..)
  , minutesIntervalToInt
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default
import           Data.Map                    (Map)
import           Data.Maybe
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time
import           GHCJS.DOM.Element
import           Language.Javascript.JSaddle hiding (create)
import           Reflex.Dom.Core             hiding (Element, fromJSString)
import           Reflex.Dom.DHTMLX.Common    (DhtmlxCalendar,
                                              MinutesInterval (..),
                                              WeekDay (..),
                                              calendarConfig_button,
                                              calendarConfig_input,
                                              calendarConfig_minutesInterval,
                                              calendarConfig_parent,
                                              calendarConfig_weekStart,
                                              dateWidgetHide, dateWidgetShow,
                                              minutesIntervalToInt,
                                              setDateFormat, setFormattedDate,
                                              setMinutesInterval, setPosition,
                                              withCalendar)

------------------------------------------------------------------------------

newtype DateTimeWidgetRef = DateTimeWidgetRef DhtmlxCalendar
  deriving (ToJSVal, MakeObject)


dateTimeFormat :: String
dateTimeFormat = "%Y-%m-%d %H:%M"


calendarsDateTimeFormat :: String
calendarsDateTimeFormat = "%Y-%m-%d %H:%i"


dateTimeFormatter :: UTCTime -> String
dateTimeFormatter = formatTime defaultTimeLocale dateTimeFormat


------------------------------------------------------------------------------
getDateTimeWidgetValue :: MonadJSM m => DateTimeWidgetRef -> m Text
getDateTimeWidgetValue a
  = liftJSM $ valToText =<< a ^. js1 "getFormatedDate" calendarsDateTimeFormat


------------------------------------------------------------------------------
dateWidgetUpdates
    :: (TriggerEvent t m, MonadJSM m) => DateTimeWidgetRef -> m (Event t Text, Event t Text)
dateWidgetUpdates cal = do
    (event, trigger) <- newTriggerEvent
    let onClickCB = fun $ \_ _ _ -> do
          txt <- getDateTimeWidgetValue cal
          liftIO $ trigger txt
    void $ liftJSM $ cal ^. js2 "attachEvent" "onClick" onClickCB

    (timeEvent, timeTrigger) <- newTriggerEvent
    let onTimeChangeCB = fun $ \_ _ _ -> do
          txt <- getDateTimeWidgetValue cal
          liftIO $ timeTrigger txt
    void $ liftJSM $ cal ^. js2 "attachEvent" "onTimeChange" onTimeChangeCB
    return (event, timeEvent)

------------------------------------------------------------------------------
data DateTimePickerConfig t = DateTimePickerConfig
    { _dateTimePickerConfig_initialValue    :: Maybe UTCTime
    , _dateTimePickerConfig_setValue        :: Event t (Maybe UTCTime)
    , _dateTimePickerConfig_button          :: Maybe Element
    , _dateTimePickerConfig_parent          :: Maybe Element
    , _dateTimePickerConfig_weekStart       :: WeekDay
    , _dateTimePickerConfig_minutesInterval :: MinutesInterval
    , _dateTimePickerConfig_attributes      :: Dynamic t (Map Text Text)
    , _dateTimePickerConfig_visibleOnLoad   :: Bool
    , _dateTimePickerConfig_timeZone        :: TimeZone
    -- | a function from the date update event to an event that hides the date picker. Defaulted to id.
    , _dateTimePickerConfig_hideRule        :: Event t () -> Event t ()
    }

makeLenses ''DateTimePickerConfig

instance Reflex t => Default (DateTimePickerConfig t) where
    def = DateTimePickerConfig Nothing never Nothing Nothing Sunday Minutes1 mempty False utc id

instance HasAttributes (DateTimePickerConfig t) where
  type Attrs (DateTimePickerConfig t) = Dynamic t (Map Text Text)
  attributes = dateTimePickerConfig_attributes

newtype DateTimePicker t = DateTimePicker
    { _dateTimePicker_value :: Dynamic t (Maybe UTCTime)
    }

instance HasValue (DateTimePicker t) where
    type Value (DateTimePicker t) = Dynamic t (Maybe UTCTime)
    value = _dateTimePicker_value

------------------------------------------------------------------------------
dhtmlxDateTimePicker
    :: forall t m. MonadWidget t m
    => DateTimePickerConfig t
    -> m (DateTimePicker t)
dhtmlxDateTimePicker (DateTimePickerConfig iv sv b p wstart mint attrs visibleOnLoad zone hideRule) = mdo
    let formatter = T.pack . maybe ""
          (formatTime defaultTimeLocale dateTimeFormat . utcToZonedTime zone)
        ivTxt     = formatter iv
    -- we set the text input with postBuild due to a race condition in dhtmlx-calendar
    pb <- getPostBuild
    ti <- textInput $ def
      & attributes .~ attrs
      & textInputConfig_setValue .~ leftmost [formatter <$> sv, formatter . parser <$> ups, ivTxt <$ pb]
    let dateEl = toElement $ _textInput_element ti
        config = def
            & calendarConfig_button .~ b
            & calendarConfig_parent .~ p
            & calendarConfig_input ?~ dateEl
            & calendarConfig_minutesInterval .~ mint
            & calendarConfig_weekStart .~ wstart
    ups <- withCalendar config $ \cal -> do
      when visibleOnLoad (dateWidgetShow cal)
      when (isJust p) $ setPosition cal 0 0
      setMinutesInterval cal mint
      setDateFormat cal $ T.pack calendarsDateTimeFormat
      (ups', timeups') <- dateWidgetUpdates $ DateTimeWidgetRef cal
      performEvent_ $ dateWidgetHide cal <$ hideRule (() <$ ups')
      performEvent_ $ ffor (fmapMaybe (fmap (T.pack . dateTimeFormatter)) sv) $
         setFormattedDate cal $ T.pack dateTimeFormat
      return $ leftmost [ups', timeups']
    let parser   = fmap (zonedTimeToUTC . (\dd -> dd {zonedTimeZone = zone}))
                 . parseTimeM True defaultTimeLocale dateTimeFormat . T.unpack
        evParsed = leftmost [parser <$> _textInput_input ti, parser <$> ups, sv]
    dVal <- holdDyn iv evParsed
    return $ DateTimePicker dVal

