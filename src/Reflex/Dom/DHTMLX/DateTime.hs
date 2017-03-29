{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Reflex.Dom.DHTMLX.DateTime where

------------------------------------------------------------------------------
import           Control.Lens
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
import           GHCJS.Types
#endif
import           Reflex.Dom hiding (Element, fromJSString)
import           Reflex.Dom.DHTMLX.Common
------------------------------------------------------------------------------

#ifdef ghcjs_HOST_OS
newtype DateTimeWidgetRef = DateTimeWidgetRef { unDateTimeWidgetRef :: JSVal }
#else
data DateTimeWidgetRef
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
  "(function(){\
    var cal = new dhtmlXCalendarObject($1);\
    cal.setWeekStartDay($2);\
    cal.setMinutesInterval($3);\
    cal.setDateFormat('%Y-%m-%d %H:%i');\
    cal.showTime();\
    return cal;\
   })()"
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
  "(function(){\
    var cal = new dhtmlXCalendarObject({input: $2, button: $1});\
    cal.setWeekStartDay($3);\
    cal.setMinutesInterval($4);\
    cal.setDateFormat('%Y-%m-%d %H:%i');\
    cal.showTime();\
    return cal;\
   })()"
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
dateWidgetUpdates :: MonadWidget t m => DateTimeWidgetRef -> m (Event t Text)
#ifdef ghcjs_HOST_OS
dateWidgetUpdates cal = do
    pb <- getPostBuild
    let act cb = liftIO $ do
          jscb <- asyncCallback2 $ \_ _ -> do
              d <- getDateTimeWidgetValue cal
              liftIO $ cb d
          js_addClickListener cal jscb
          js_addTimeChangeListener cal jscb
    performEventAsync (act <$ pb)

foreign import javascript unsafe
  "(function(){ $1['attachEvent'](\"onClick\", $2); })()"
  js_addClickListener :: DateTimeWidgetRef -> Callback (JSVal -> JSVal -> IO ()) -> IO ()
foreign import javascript unsafe
  "(function(){ $1['attachEvent'](\"onTimeChange\", $2); })()"
  js_addTimeChangeListener :: DateTimeWidgetRef -> Callback (JSVal -> JSVal -> IO ()) -> IO ()
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
    }

makeLenses ''DateTimePickerConfig

instance Reflex t => Default (DateTimePickerConfig t) where
    def = DateTimePickerConfig Nothing never Nothing Sunday Minutes1 mempty

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
    :: MonadWidget t m
    => DateTimePickerConfig t
    -> m (DateTimePicker t)
dhtmlxDateTimePicker (DateTimePickerConfig iv sv b wstart mint attrs) = mdo
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
    pb <- delay 0 =<< getPostBuild
    let create = maybe createDhtmlxDateTimeWidget createDhtmlxDateTimeWidgetButton b
    calRef <- performEvent (liftIO (create dateEl wstart mint) <$ pb)
    res <- widgetHold (return never) $ dateWidgetUpdates <$> calRef
    let ups = switchPromptlyDyn res
    let parser = parseTimeM True defaultTimeLocale fmt . T.unpack
    fmap DateTimePicker $ holdDyn iv $ leftmost
      [ parser <$> _textInput_input ti
      , parser <$> ups
      , sv
      ]
