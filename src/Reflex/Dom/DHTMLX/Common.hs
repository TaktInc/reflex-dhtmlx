{-# LANGUAGE CPP #-}
module Reflex.Dom.DHTMLX.Common where

#ifdef ghcjs_HOST_OS
import Control.Monad.IO.Class
import GHCJS.Types
#endif

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

#ifdef ghcjs_HOST_OS
dateWidgetShow :: MonadIO m  => JSVal -> m ()
dateWidgetShow = liftIO . js_dateWidgetShow

foreign import javascript unsafe
  "(function(){ $1['show'](); })()"
  js_dateWidgetShow :: JSVal -> IO ()
#else
dateWidgetShow :: Monad m  => a -> m ()
dateWidgetShow _ = return ()
#endif
