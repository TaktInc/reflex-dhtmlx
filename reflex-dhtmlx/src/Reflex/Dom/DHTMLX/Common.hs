{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Reflex.Dom.DHTMLX.Common
  ( DhtmlxCalendar
  , WeekDay(..)
  , MinutesInterval (..)
  , calendarConfig_button
  , calendarConfig_input
  , calendarConfig_minutesInterval
  , calendarConfig_parent
  , calendarConfig_weekStart
  , dateWidgetHide, dateWidgetShow
  , setMinutesInterval
  , setPosition
  , withCalendar
  , showTime, hideTime
  , setDate
  , setFormattedDate, setDateFormat
  , minutesIntervalToInt
  ) where

import           Control.Lens
import           Control.Monad
import           Data.Default
import           Data.Text                   (Text)
import           GHCJS.DOM.Element
import           Language.Javascript.JSaddle
import           Reflex.Dom.Core             hiding (Element)

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

dateWidgetShow :: MonadJSM m  => DhtmlxCalendar -> m ()
dateWidgetShow dw = liftJSM $ void $ dw ^. js0 "show"

dateWidgetHide :: MonadJSM m  =>  DhtmlxCalendar -> m ()
dateWidgetHide dw = liftJSM $ void $ dw ^. js0 "hide"

setWeekStartDay :: MonadJSM m => DhtmlxCalendar -> WeekDay -> m ()
setWeekStartDay cal weekStart = liftJSM $ void $ cal ^. js1 "setWeekStartDay" (weekDayToInt weekStart)

setMinutesInterval :: MonadJSM m => DhtmlxCalendar -> MinutesInterval -> m ()
setMinutesInterval cal mint = liftJSM $ void $ cal ^. js1 "setMinutesInterval" (minutesIntervalToInt mint)

setPosition :: MonadJSM m => DhtmlxCalendar -> Int -> Int -> m ()
setPosition cal x y = liftJSM $ void $ cal ^. js2 "setPosition" x y

setDateFormat :: MonadJSM m => DhtmlxCalendar -> Text -> m ()
setDateFormat cal fmt = liftJSM $ void $ cal ^. js1 "setDateFormat" fmt

setDate :: MonadJSM m => DhtmlxCalendar -> Text -> m ()
setDate cal date = liftJSM $ void $ cal ^. js1 "setDate" date

setFormattedDate :: MonadJSM m => DhtmlxCalendar -> Text -> Text -> m ()
setFormattedDate cal fmt date = liftJSM $ void $ cal ^. js2 "setFormatedDate" fmt date

showTime :: MonadJSM m => DhtmlxCalendar -> m ()
showTime cal = liftJSM $ void $ cal ^. js0 "showTime"

hideTime :: MonadJSM m => DhtmlxCalendar -> m ()
hideTime cal = liftJSM $ void $ cal ^. js0 "hideTime"

attachObj :: MonadJSM m => DhtmlxCalendar -> Maybe Element -> Maybe Element -> m ()
attachObj cal input btn = liftJSM $ void $ cal ^. js1 "attachObj" (buildInputArgs input btn)

buildInputArgs :: Maybe Element -> Maybe Element -> JSM Object
buildInputArgs input btn = liftJSM $ do
    o <- obj
    mapM_ (o <# "input") input
    mapM_ (o <# "button") btn
    return o

js_dhtmlXCalendarObject :: JSM JSVal
js_dhtmlXCalendarObject = jsg "dhtmlXCalendarObject"

newtype DhtmlxCalendar = DhtmlxCalendar JSVal
  deriving (ToJSVal, MakeObject)

createDhtmlxCalendar
  :: CalendarConfig
  -> JSM DhtmlxCalendar
createDhtmlxCalendar config = do
    let createCal v = DhtmlxCalendar <$> new js_dhtmlXCalendarObject v
    cal <- case _calendarConfig_parent config of
      Nothing -> do
        no <- toJSVal ValUndefined
        cal <- createCal no
        attachObj cal (_calendarConfig_input config) (_calendarConfig_button config)
        return cal
      Just parent -> do
        cal <- createCal (toJSVal parent)
        attachObj cal (_calendarConfig_input config) (_calendarConfig_button config)
        return cal
    setMinutesInterval cal (_calendarConfig_minutesInterval config)
    setWeekStartDay cal (_calendarConfig_weekStart config)
    return cal

withCalendar
  :: MonadWidget t m
  => CalendarConfig
  -> (DhtmlxCalendar -> m (Event t a))
  -> m (Event t a)
withCalendar config f = do
    postBuild <- getPostBuild
    calE <- performEvent $ liftJSM (createDhtmlxCalendar config) <$ postBuild
    fmap (switch . current) $ widgetHold (return never) $ f <$> calE

data MinutesInterval = Minutes1 | Minutes5 | Minutes10 | Minutes15
  deriving (Eq,Ord,Show,Read,Enum,Bounded)

minutesIntervalToInt :: MinutesInterval -> Int
minutesIntervalToInt Minutes1  = 1
minutesIntervalToInt Minutes5  = 5
minutesIntervalToInt Minutes10 = 10
minutesIntervalToInt Minutes15 = 15

data CalendarConfig = CalendarConfig
    { _calendarConfig_parent          :: Maybe Element
    , _calendarConfig_input           :: Maybe Element
    , _calendarConfig_button          :: Maybe Element
    , _calendarConfig_weekStart       :: WeekDay
    , _calendarConfig_minutesInterval :: MinutesInterval
    }

instance Default CalendarConfig where
  def = CalendarConfig Nothing Nothing Nothing Monday Minutes1

makeLenses ''CalendarConfig
