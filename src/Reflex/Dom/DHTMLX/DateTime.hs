{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Reflex.Dom.DHTMLX.DateTime where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default
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
  { unDateTimeWidgetRef :: JSVal }
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
    cal <- js_createDhtmlxCalendar btnElmt elmt wstart
    void $ cal ^. js1 "setMinutesInterval" (minutesIntervalToInt mint)
    void $ cal ^. js1 "setDateFormat" "%Y-%m-%d %H:%i"
    void $ cal ^. js0 "showTime"
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

data MinutesInterval = Minutes1 | Minutes5 | Minutes10 | Minutes15
  deriving (Eq,Ord,Show,Read,Enum,Bounded)

minutesIntervalToInt :: MinutesInterval -> Int
minutesIntervalToInt Minutes1 = 1
minutesIntervalToInt Minutes5 = 5
minutesIntervalToInt Minutes10 = 10
minutesIntervalToInt Minutes15 = 15


------------------------------------------------------------------------------
data DateTimePickerConfig t = DateTimePickerConfig
    { _dateTimePickerConfig_initialValue    :: Maybe UTCTime
    , _dateTimePickerConfig_setValue        :: Event t (Maybe UTCTime)
    , _dateTimePickerConfig_button          :: Maybe Element
    , _dateTimePickerConfig_weekStart       :: WeekDay
    , _dateTimePickerConfig_minutesInterval :: MinutesInterval
    , _dateTimePickerConfig_attributes      :: Dynamic t (Map Text Text)
    , _dateTimePickerConfig_visibleOnLoad   :: Bool
    }

makeLenses ''DateTimePickerConfig

instance Reflex t => Default (DateTimePickerConfig t) where
    def = DateTimePickerConfig Nothing never Nothing Sunday Minutes1 mempty False

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
dhtmlxDateTimePicker (DateTimePickerConfig iv sv b wstart mint attrs visibleOnLoad) = mdo
    let fmt = "%Y-%m-%d %H:%M"
        formatter = T.pack . maybe "" (formatTime defaultTimeLocale fmt)
    ti <- textInput $ def
      & attributes .~ attrs
      & textInputConfig_initialValue .~ formatter iv
      & textInputConfig_setValue .~ leftmost
          [ fmap formatter sv
          , ups
          ]
    let dateEl = toElement $ _textInput_element ti
    let create p f = do
         calRef <- liftJSM $ f dateEl wstart mint
         when p $ dateWidgetShow $ unDateTimeWidgetRef calRef
         dateWidgetUpdates calRef
    ups <- case b of
      Nothing | visibleOnLoad -> create True createDhtmlxDateTimeWidget
      Nothing -> do
        lazyCreate <- headE $ create False createDhtmlxDateTimeWidget <$ domEvent Focus ti
        fmap (switch . current) $ widgetHold (return never) lazyCreate
      Just b' | visibleOnLoad -> create True $ createDhtmlxDateTimeWidgetButton b'
      Just b' -> do
        b'' <- wrapRawElement (toElement b') def
        lazyCreate <- headE $ leftmost
          [ create False (createDhtmlxDateTimeWidgetButton b') <$ domEvent Focus ti
          , create True (createDhtmlxDateTimeWidgetButton b') <$ domEvent Click b''
          ]
        fmap (switch . current) $ widgetHold (return never) lazyCreate
    let parser = parseTimeM True defaultTimeLocale fmt . T.unpack
    fmap DateTimePicker $ holdDyn iv $ leftmost
      [ parser <$> _textInput_input ti
      , parser <$> ups
      , sv
      ]
