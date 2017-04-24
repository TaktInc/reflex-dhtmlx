{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Reflex.Dom.DHTMLX.Date where

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
#endif
import           Reflex.Dom hiding (Element, fromJSString)
import           Reflex.Dom.DHTMLX.Common
------------------------------------------------------------------------------

newtype DateWidgetRef = DateWidgetRef { unDateWidgetRef :: JSVal }

------------------------------------------------------------------------------
createDhtmlxDateWidget :: Element -> WeekDay -> IO DateWidgetRef
#ifdef ghcjs_HOST_OS
createDhtmlxDateWidget elmt wstart =
    js_createDhtmlxDateWidget (pToJSVal elmt) (weekDayToInt wstart)

foreign import javascript unsafe
  "(function(){\
    var cal = new dhtmlXCalendarObject($1);\
    cal.setWeekStartDay($2);\
    cal.hideTime();\
    return cal;\
   })()"
  js_createDhtmlxDateWidget :: JSVal -> Int -> IO DateWidgetRef

#else
createDhtmlxDateWidget = error "createDhtmlxDateWidget: can only be used with GHCJS"
#endif


------------------------------------------------------------------------------
createDhtmlxDateWidgetButton
    :: Element
    -> Element
    -> WeekDay
    -> IO DateWidgetRef
#ifdef ghcjs_HOST_OS
createDhtmlxDateWidgetButton b elmt wstart =
    js_createDhtmlxDateWidgetButton (pToJSVal b) (pToJSVal elmt)
                              (weekDayToInt wstart)

foreign import javascript unsafe
  "(function(){\
    var cal = new dhtmlXCalendarObject({input: $2, button: $1});\
    cal.setWeekStartDay($3);\
    cal.hideTime();\
    return cal;\
   })()"
  js_createDhtmlxDateWidgetButton :: JSVal -> JSVal -> Int -> IO DateWidgetRef

#else
createDhtmlxDateWidgetButton =
    error "createDhtmlxDateWidgetButton: can only be used with GHCJS"
#endif


------------------------------------------------------------------------------
getDateWidgetValue :: MonadIO m => DateWidgetRef -> m Text
#ifdef ghcjs_HOST_OS
getDateWidgetValue a = liftIO $ fromJSString <$> js_getDateWidgetValue a

foreign import javascript unsafe
  "(function(){ return $1['getDate'](true); })()"
  js_getDateWidgetValue :: DateWidgetRef -> IO JSString

#else
getDateWidgetValue = error "getDateWidgetValue: can only be used with GHCJS"
#endif

------------------------------------------------------------------------------
dateWidgetUpdates
    :: (TriggerEvent t m, MonadIO m)
    => DateWidgetRef
    -> m (Event t Text)
#ifdef ghcjs_HOST_OS
dateWidgetUpdates cal = do
    (event, trigger) <- newTriggerEvent
    jscb <- liftIO $ asyncCallback $ trigger =<< getDateWidgetValue cal
    liftIO $ js_addClickListener cal jscb
    return event

foreign import javascript unsafe
  "(function(){ $1['attachEvent'](\"onClick\", $2); })()"
  js_addClickListener :: DateWidgetRef -> Callback (IO ()) -> IO ()
#else
dateWidgetUpdates = error "dateWidgetUpdates: can only be used with GHCJS"
#endif


------------------------------------------------------------------------------
data DatePickerConfig t = DatePickerConfig
    { _datePickerConfig_initialValue  :: Maybe Day
    , _datePickerConfig_setValue      :: Event t (Maybe Day)
    , _datePickerConfig_button        :: Maybe Element
    , _datePickerConfig_weekStart     :: WeekDay
    , _datePickerConfig_attributes    :: Dynamic t (Map Text Text)
    , _datePickerConfig_visibleOnLoad :: Bool
    }

makeLenses ''DatePickerConfig

instance Reflex t => Default (DatePickerConfig t) where
    def = DatePickerConfig Nothing never Nothing Sunday mempty False

instance HasAttributes (DatePickerConfig t) where
  type Attrs (DatePickerConfig t) = Dynamic t (Map Text Text)
  attributes = datePickerConfig_attributes

newtype DatePicker t = DatePicker
    { _datePicker_value :: Dynamic t (Maybe Day)
    }

instance HasValue (DatePicker t) where
    type Value (DatePicker t) = Dynamic t (Maybe Day)
    value = _datePicker_value

------------------------------------------------------------------------------
dhtmlxDatePicker
    :: MonadWidget t m
    => DatePickerConfig t
    -> m (DatePicker t)
dhtmlxDatePicker (DatePickerConfig iv sv b wstart attrs visibleOnLoad) = do
    let fmt = "%Y-%m-%d"
        formatter = T.pack . maybe "" (formatTime defaultTimeLocale fmt)
    ti <- textInput $ def
      & attributes .~ attrs
      & textInputConfig_initialValue .~ formatter iv
      & textInputConfig_setValue .~ fmap formatter sv
    let dateEl = toElement $ _textInput_element ti
    let create p f = do
          calRef <- liftIO $ f dateEl wstart
          when p $ dateWidgetShow $ unDateWidgetRef calRef
          dateWidgetUpdates calRef
    ups <- case b of
      Nothing | visibleOnLoad -> create True createDhtmlxDateWidget
      Nothing -> do
        lazyCreate <- headE $ create False createDhtmlxDateWidget <$ domEvent Focus ti
        fmap (switch . current) $ widgetHold (return never) lazyCreate
      Just b' | visibleOnLoad -> create True $ createDhtmlxDateWidgetButton b'
      Just b' -> do
#ifdef ghcjs_HOST_OS
        click' <- wrapDomEvent b' (`on` Element.click) $ return ()
#else
        let click' = never
#endif
        lazyCreate <- headE $ leftmost
          [ create False (createDhtmlxDateWidgetButton b') <$ domEvent Focus ti
          , create True (createDhtmlxDateWidgetButton b') <$ click'
          ]
        fmap (switch . current) $ widgetHold (return never) lazyCreate
    let parser = parseTimeM True defaultTimeLocale fmt . T.unpack
    fmap DatePicker $ holdDyn iv $ leftmost
      [ parser <$> _textInput_input ti
      , parser <$> ups
      , sv
      ]
