module Reflex.Dom.DHTMLX.Common where

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

