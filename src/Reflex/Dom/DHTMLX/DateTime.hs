{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module Reflex.Dom.DHTMLX.DateTime where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans
import           Data.Default
import           Data.Map                (Map)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Time
import           GHCJS.DOM.Types hiding (Event, Text)
#ifdef ghcjs_HOST_OS
import           GHCJS.Marshal.Pure (pToJSVal)
import           GHCJS.Foreign.Callback
import qualified GHCJS.DOM.Element as Element
import           GHCJS.DOM.EventM (on)
import           GHCJS.Types (JSString, JSVal)
#endif
import           Reflex.Dom hiding (Element, fromJSString)
import           Reflex.Dom.DHTMLX.Common
------------------------------------------------------------------------------

newtype DateTimeWidgetRef = DateTimeWidgetRef
#ifdef ghcjs_HOST_OS
    { unDateTimeWidgetRef :: JSVal }
#else
    { unDateTimeWidgetRef :: () }
#endif

------------------------------------------------------------------------------
createDhtmlxDateTimeWidget
    :: Element
    -> WeekDay
    -> MinutesInterval
    -> IO DateTimeWidgetRef
#ifdef ghcjs_HOST_OS
createDhtmlxDateTimeWidget elmt wstart mint =
    js_createDhtmlxDateTimeWidget (pToJSVal elmt) (weekDayToInt wstart)
                                  (minutesIntervalToInt mint)

foreign import javascript unsafe
  "(function(dhtmlXCalendarObject){\
    var cal = new dhtmlXCalendarObject($1);\
    cal['setWeekStartDay']($2);\
    cal['setMinutesInterval']($3);\
    cal['setDateFormat']('%Y-%m-%d %H:%i');\
    cal['showTime']();\
    return cal;\
   })(window['dhtmlXCalendarObject'])"
  js_createDhtmlxDateTimeWidget :: JSVal -> Int -> Int -> IO DateTimeWidgetRef

#else
createDhtmlxDateTimeWidget =
    error "createDhtmlxDateTimeWidget: can only be used with GHCJS"
#endif


------------------------------------------------------------------------------
createDhtmlxDateTimeWidgetButton
    :: Element
    -> Element
    -> WeekDay
    -> MinutesInterval
    -> IO DateTimeWidgetRef
#ifdef ghcjs_HOST_OS
createDhtmlxDateTimeWidgetButton b elmt wstart mint =
    js_createDhtmlxDateTimeWidgetButton (pToJSVal b) (pToJSVal elmt)
                                        (weekDayToInt wstart)
                                        (minutesIntervalToInt mint)

foreign import javascript unsafe
  "(function(dhtmlXCalendarObject){\
    var cal = new dhtmlXCalendarObject({input: $2, button: $1});\
    cal['setWeekStartDay']($3);\
    cal['setMinutesInterval']($4);\
    cal['setDateFormat']('%Y-%m-%d %H:%i');\
    cal['showTime']();\
    return cal;\
   })(window['dhtmlXCalendarObject'])"
  js_createDhtmlxDateTimeWidgetButton :: JSVal -> JSVal -> Int -> Int -> IO DateTimeWidgetRef

#else
createDhtmlxDateTimeWidgetButton = error "createDhtmlxDateTimeWidgetButton: can only be used with GHCJS"
#endif


------------------------------------------------------------------------------
getDateTimeWidgetValue :: MonadIO m => DateTimeWidgetRef -> m Text
#ifdef ghcjs_HOST_OS
getDateTimeWidgetValue a = liftIO $ fromJSString <$> js_getDateTimeWidgetValue a

foreign import javascript unsafe
  "(function(){ return $1['getFormatedDate']('%Y-%m-%d %H:%i'); })()"
  js_getDateTimeWidgetValue :: DateTimeWidgetRef -> IO JSString
#else
getDateTimeWidgetValue = error "getDateTimeWidgetValue: can only be used with GHCJS"
#endif


------------------------------------------------------------------------------
dateWidgetUpdates
    :: (TriggerEvent t m, MonadIO m) => DateTimeWidgetRef -> m (Event t Text)
#ifdef ghcjs_HOST_OS
dateWidgetUpdates cal = do
    (event, trigger) <- newTriggerEvent
    jscb <- liftIO $ asyncCallback $ trigger =<< getDateTimeWidgetValue cal
    liftIO $ js_addClickListener cal jscb
    liftIO $ js_addTimeChangeListener cal jscb
    return event

foreign import javascript unsafe
  "(function(){ $1['attachEvent']('onClick', $2); })()"
  js_addClickListener :: DateTimeWidgetRef -> Callback (IO ()) -> IO ()
foreign import javascript unsafe
  "(function(){ $1['attachEvent']('onTimeChange', $2); })()"
  js_addTimeChangeListener :: DateTimeWidgetRef -> Callback (IO ()) -> IO ()
#else
dateWidgetUpdates = error "dateWidgetUpdates: can only be used with GHCJS"
#endif

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
         calRef <- liftIO $ f dateEl wstart mint
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
