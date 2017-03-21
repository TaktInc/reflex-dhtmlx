{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}

module Reflex.Dom.DHTMLX.DateTime where

------------------------------------------------------------------------------
import           Control.Monad.Trans
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Time
#ifdef ghcjs_HOST_OS
import           GHCJS.DOM.Types hiding (Event, Text)
import           GHCJS.Marshal.Pure (pToJSVal)
import           GHCJS.Foreign.Callback
import           GHCJS.Types
#endif
import           Reflex.Dom hiding (Element, fromJSString)
------------------------------------------------------------------------------

newtype DateTimeWidgetRef = DateTimeWidgetRef { unDateTimeWidgetRef :: JSVal }

------------------------------------------------------------------------------
createDhtmlxDateTimeWidget :: Element -> IO DateTimeWidgetRef
#ifdef ghcjs_HOST_OS
createDhtmlxDateTimeWidget elmt = js_createDhtmlxDateTimeWidget (pToJSVal elmt)

foreign import javascript unsafe
  "(function(){\
    var cal = new dhtmlXCalendarObject($1);\
    cal.setWeekStartDay(7);\
    cal.setMinutesInterval(1);\
    cal.setDateFormat('%Y-%m-%d %H:%i');\
    cal.showTime();\
    return cal;\
   })()"
  js_createDhtmlxDateTimeWidget :: JSVal -> IO DateTimeWidgetRef

#else
createDhtmlxDateTimeWidget = error "createDhtmlxDateTimeWidget: can only be used with GHCJS"
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


------------------------------------------------------------------------------
dhtmlxDateTimePicker
    :: MonadWidget t m
    => Maybe UTCTime
    -> Event t (Maybe UTCTime)
    -> m (Dynamic t (Maybe UTCTime))
dhtmlxDateTimePicker iv sv = mdo
    let fmt = "%Y-%m-%d %H:%M"
        formatter = T.pack . maybe "" (formatTime defaultTimeLocale fmt)
    ti <- textInput $ def
      & textInputConfig_initialValue .~ formatter iv
      & textInputConfig_setValue .~ leftmost
          [ fmap formatter sv
          , ups
          ]
    let dateEl = toElement $ _textInput_element ti
    pb <- delay 0 =<< getPostBuild
    calRef <- performEvent (liftIO (createDhtmlxDateTimeWidget dateEl) <$ pb)
    res <- widgetHold (return never) $ dateWidgetUpdates <$> calRef
    let ups = switchPromptlyDyn res
    let parser = parseTimeM True defaultTimeLocale fmt . T.unpack
    holdDyn iv $ leftmost
      [ parser <$> _textInput_input ti
      , parser <$> ups
      , sv
      ]
