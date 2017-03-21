{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}

module Reflex.Dom.DHTMLX.Date where

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

newtype DateWidgetRef = DateWidgetRef { unDateWidgetRef :: JSVal }

------------------------------------------------------------------------------
createDhtmlxDateWidget :: Element -> IO DateWidgetRef
#ifdef ghcjs_HOST_OS
createDhtmlxDateWidget elmt = js_createDhtmlxDateWidget (pToJSVal elmt)

foreign import javascript unsafe
  "(function(){\
    var cal = new dhtmlXCalendarObject($1);\
    cal.setWeekStartDay(7);\
    cal.hideTime();\
    return cal;\
   })()"
  js_createDhtmlxDateWidget :: JSVal -> IO DateWidgetRef

#else
createDhtmlxDateWidget = error "createDhtmlxDateWidget: can only be used with GHCJS"
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
dateWidgetUpdates :: MonadWidget t m => DateWidgetRef -> m (Event t Text)
#ifdef ghcjs_HOST_OS
dateWidgetUpdates cal = do
    pb <- getPostBuild
    let act cb = liftIO $ do
          jscb <- asyncCallback2 $ \_ _ -> do
              d <- getDateWidgetValue cal
              liftIO $ cb d
          js_addClickListener cal jscb
--          js_addChangeListener cal jscb
--          js_addTimeChangeListener cal jscb
    performEventAsync (act <$ pb)

foreign import javascript unsafe
  "(function(){ $1['attachEvent'](\"onClick\", $2); })()"
  js_addClickListener :: DateWidgetRef -> Callback (JSVal -> JSVal -> IO ()) -> IO ()
--foreign import javascript unsafe
--  "(function(){ $1['attachEvent'](\"onChange\", $2); })()"
--  js_addChangeListener :: DateWidgetRef -> Callback (JSVal -> JSVal -> IO ()) -> IO ()
--foreign import javascript unsafe
--  "(function(){ $1['attachEvent'](\"onTimeChange\", $2); })()"
--  js_addTimeChangeListener :: DateWidgetRef -> Callback (JSVal -> JSVal -> IO ()) -> IO ()
#else
dateWidgetUpdates = error "dateWidgetUpdates: can only be used with GHCJS"
#endif


------------------------------------------------------------------------------
dhtmlxDatePicker
    :: MonadWidget t m
    => Maybe Day
    -> Event t (Maybe Day)
    -> m (Dynamic t (Maybe Day))
dhtmlxDatePicker iv sv = do
    let fmt = "%Y-%m-%d"
        formatter = T.pack . maybe "" (formatTime defaultTimeLocale fmt)
    ti <- textInput $ def
      & textInputConfig_initialValue .~ formatter iv
      & textInputConfig_setValue .~ fmap formatter sv
    let dateEl = toElement $ _textInput_element ti
    pb <- delay 0 =<< getPostBuild
    calRef <- performEvent (liftIO (createDhtmlxDateWidget dateEl) <$ pb)
    ups <- widgetHold (return never) $ dateWidgetUpdates <$> calRef
    let parser = parseTimeM True defaultTimeLocale fmt . T.unpack
    holdDyn iv $ leftmost
      [ parser <$> _textInput_input ti
      , parser <$> switchPromptlyDyn ups
      , sv
      ]
