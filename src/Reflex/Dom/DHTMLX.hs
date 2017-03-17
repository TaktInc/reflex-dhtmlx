{-# LANGUAGE TemplateHaskell #-}

module Reflex.Dom.DHTMLX where

import Data.Time
#ifdef ghcjs_HOST_OS
import           GHCJS.DOM.Types hiding (Event, Text)
import           GHCJS.Marshal.Pure (pToJSVal)
import           GHCJS.Foreign
import           GHCJS.Foreign.Callback
import           GHCJS.Types
#endif
import Reflex.Dom hiding (Element)

newtype CalendarRef = CalendarRef { unCalendarRef :: JSVal }

------------------------------------------------------------------------------
createDhtmlxCalendar :: Element -> IO ()
createDhtmlxCalendar = js_createDhtmlxCalendar . toJSString

FOREIGN_IMPORT(unsafe, js_createDhtmlxCalendar, Element -> IO CalendarRef, "(function(){}return new dhtmlXCalendarObject($1);)()")

dhtmlxDatePicker
    :: MonadWidget t m
    => Maybe Day
    -> Event t (Maybe Day)
    -> m (Dynamic t (Maybe Day))
dhtmlxDatePicker iv sv = do
    (dateDiv,_) <- elAttr' "input" ("type" =: "text" <> "id" =: "cal_1") blank
    let dateEl = _element_raw dateDiv
    pb <- getPostBuild
    dateUpdates <- performEvent (liftIO (createDhtmlxCalendar dateEl) <$ pb)
    return $ constDyn Nothing
