module Reflex.Dom.DHTMLX.Common where

import Control.Monad
import Control.Lens
import GHCJS.DOM.Element
import Language.Javascript.JSaddle

data WeekDay
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
  deriving (Eq,Ord,Show,Read,Enum,Bounded)

-- | Converts a WeekDay into an int according to the scheme required by
-- setWeekStartDay.
weekDayToInt :: WeekDay -> Int
weekDayToInt d = fromEnum d + 1

dateWidgetShow :: MonadJSM m  => JSVal -> m ()
dateWidgetShow dw = liftJSM $ void $ dw ^. js0 "show"

js_dhtmlXCalendarObject :: MonadJSM m => m JSVal
js_dhtmlXCalendarObject = liftJSM $ jsg "dhtmlXCalendarObject"

js_createDhtmlxCalendar
    :: Maybe Element
    -> Element
    -> WeekDay
    -> JSM JSVal
js_createDhtmlxCalendar btnElmt elmt wstart = do
    args <- obj
    (args <# "input") elmt
    mapM_ (args <# "button") btnElmt
    calendarObj <- js_dhtmlXCalendarObject
    cal <- new calendarObj $ pToJSVal elmt
    void $ cal ^. js1 "setWeekStartDay" (weekDayToInt wstart)
    return cal
