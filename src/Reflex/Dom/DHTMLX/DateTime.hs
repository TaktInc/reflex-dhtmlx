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
  ( module Reflex.Dom.DHTMLX.DateTime
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
import           Reflex.Dom                  hiding (Element, fromJSString)
import           Reflex.Dom.DHTMLX.Common
------------------------------------------------------------------------------

newtype DateTimeWidgetRef = DateTimeWidgetRef
  { unDateTimeWidgetRef :: DhtmlxCalendar }
  deriving (ToJSVal, MakeObject)


------------------------------------------------------------------------------
createDhtmlxDateTimeWidget
    :: Element
    -> WeekDay
    -> MinutesInterval
    -> JSM DateTimeWidgetRef
createDhtmlxDateTimeWidget = createDhtmlxDateTimeWidget' Nothing

------------------------------------------------------------------------------
createDhtmlxDateTimeWidgetButton
    :: Element
    -> Element
    -> WeekDay
    -> MinutesInterval
    -> JSM DateTimeWidgetRef
createDhtmlxDateTimeWidgetButton btnElmt = createDhtmlxDateTimeWidget' (Just btnElmt)

------------------------------------------------------------------------------
createDhtmlxDateTimeWidget'
    :: Maybe Element
    -> Element
    -> WeekDay
    -> MinutesInterval
    -> JSM DateTimeWidgetRef
createDhtmlxDateTimeWidget' btnElmt elmt wstart mint = do
    let config = def
          & calendarConfig_button .~ btnElmt
          & calendarConfig_input ?~ elmt
          & calendarConfig_weekStart .~ wstart
    cal <- createDhtmlxCalendar config
    setMinutesInterval cal mint
    setDateFormat cal $ T.pack "%Y-%m-%d %H:%i"
    showTime cal
    return $ DateTimeWidgetRef cal

------------------------------------------------------------------------------
getDateTimeWidgetValue :: MonadJSM m => DateTimeWidgetRef -> m Text
getDateTimeWidgetValue a = liftJSM $ valToText =<< a ^. js1 "getFormatedDate" "%Y-%m-%d %H:%i"


------------------------------------------------------------------------------
dateWidgetUpdates
    :: (TriggerEvent t m, MonadJSM m) => DateTimeWidgetRef -> m (Event t Text)
dateWidgetUpdates cal = do
    (event, trigger) <- newTriggerEvent
    void $ liftJSM $ cal ^. js2 "attachEvent" "onClick" (fun $ \_ _ _ -> liftIO . trigger =<< getDateTimeWidgetValue cal)
    void $ liftJSM $ cal ^. js2 "attachEvent" "onTimeChange" (fun $ \_ _ _ -> liftIO . trigger =<< getDateTimeWidgetValue cal)
    return event

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
    }

makeLenses ''DateTimePickerConfig

instance Reflex t => Default (DateTimePickerConfig t) where
    def = DateTimePickerConfig Nothing never Nothing Nothing Sunday Minutes1 mempty False

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
dhtmlxDateTimePicker (DateTimePickerConfig iv sv b p wstart mint attrs visibleOnLoad) = mdo
    zone <- liftIO getCurrentTimeZone
    let fmt = "%Y-%m-%d %H:%M"
        formatter = T.pack . maybe "" (formatTime defaultTimeLocale fmt . utcToZonedTime zone)
    ti <- textInput $ def
      & attributes .~ attrs
      & textInputConfig_initialValue .~ formatter iv
      & textInputConfig_setValue .~ leftmost [fmap formatter sv, ups]
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
      ups' <- dateWidgetUpdates $ DateTimeWidgetRef cal
      performEvent_ $ dateWidgetHide cal <$ ups'
      return ups'
    let parser = fmap zonedTimeToUTC . parseTimeM True defaultTimeLocale fmt . T.unpack
    fmap DateTimePicker $ holdDyn iv $ parser <$> leftmost [_textInput_input ti, ups]
