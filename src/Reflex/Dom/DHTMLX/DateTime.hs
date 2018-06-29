{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleContexts           #-}
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
import           Data.Maybe
import           Data.Map                (Map)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Time
import           GHCJS.DOM.Element
import           Language.Javascript.JSaddle hiding (create)
import           Reflex.Dom hiding (Element, fromJSString)
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


dateTimeFormat :: String
dateTimeFormat = "%Y-%m-%d %H:%M"
-- "%Y-%m-%d %H:%M"


calendarsDateTimeFormat :: String
calendarsDateTimeFormat = "%Y-%m-%d %H:%i"


dateTimeFormatter :: UTCTime -> String
dateTimeFormatter = formatTime defaultTimeLocale dateTimeFormat


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
          & calendarConfig_input .~ Just elmt
          & calendarConfig_weekStart .~ wstart
    cal <- createDhtmlxCalendar config
    setMinutesInterval cal mint
    setDateFormat cal $ T.pack calendarsDateTimeFormat
    showTime cal
    return $ DateTimeWidgetRef cal

------------------------------------------------------------------------------
getDateTimeWidgetValue :: MonadJSM m => DateTimeWidgetRef -> m Text
getDateTimeWidgetValue a
  = liftJSM $ valToText =<< a ^. js1 "getFormatedDate" calendarsDateTimeFormat


------------------------------------------------------------------------------
dateWidgetUpdates
    :: (TriggerEvent t m, MonadJSM m) => DateTimeWidgetRef -> m (Event t Text)
dateWidgetUpdates cal = do
    (event, trigger) <- newTriggerEvent
    let onClickCB = fun $ \_ _ _ -> do
          txt <- getDateTimeWidgetValue cal
          liftIO $ trigger txt
    void $ liftJSM $ cal ^. js2 "attachEvent" "onClick" onClickCB

    let onTimeChangeCB = fun $ \_ _ _ -> do
          txt <- getDateTimeWidgetValue cal
          liftIO $ trigger txt
    void $ liftJSM $ cal ^. js2 "attachEvent" "onTimeChange" onTimeChangeCB
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
    let formatter = T.pack . maybe "" dateTimeFormatter
        ivTxt     = formatter iv
        evVal     = leftmost [formatter <$> sv, ups]
    ti <- textInput $ def
      & attributes .~ attrs
      & textInputConfig_initialValue .~ ivTxt
      & textInputConfig_setValue .~ evVal
    let dateEl = toElement $ _textInput_element ti
        config = def
            & calendarConfig_button .~ b
            & calendarConfig_parent .~ p
            & calendarConfig_input .~ Just dateEl
            & calendarConfig_minutesInterval .~ mint
            & calendarConfig_weekStart .~ wstart
    ups <- withCalendar config $ \cal -> do
      when visibleOnLoad (dateWidgetShow cal)
      when (isJust p) $ setPosition cal 0 0
      ups' <- dateWidgetUpdates $ DateTimeWidgetRef cal
      performEvent_ $ dateWidgetHide cal <$ ups'
      performEvent_ $ ffor (fmapMaybe (fmap (T.pack . dateTimeFormatter)) sv) $
        setFormattedDate cal $ T.pack calendarsDateTimeFormat
      return ups'
    let parser   = parseTimeM True defaultTimeLocale dateTimeFormat . T.unpack
        evParsed = parser <$> leftmost [_textInput_input ti, ups]
    performEvent_ $ liftIO . putStrLn . ("sv  : " ++) . show <$> sv
    performEvent_ $ liftIO . putStrLn . ("fsv : " ++) . show . formatter <$> sv
    performEvent_ $ liftIO . putStrLn . ("ups : " ++) . show <$> ups
    performEvent_ $ liftIO . putStrLn . ("pups: " ++) . show . parser <$> ups
    performEvent_ $ liftIO . putStrLn . ("ti  : " ++) . show <$> _textInput_input ti
    performEvent_ $ liftIO . putStrLn . ("=   : " ++) . show <$> evParsed
    dVal    <- holdDyn iv evParsed
    return $ DateTimePicker dVal
