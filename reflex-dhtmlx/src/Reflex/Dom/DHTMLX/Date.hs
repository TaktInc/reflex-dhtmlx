{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Reflex.Dom.DHTMLX.Date
  ( dhtmlxDatePicker
  , DatePickerConfig (..)
  , DatePicker
  , _datePicker_value
  , datePickerConfig_initialValue
  , datePickerConfig_setValue
  , datePickerConfig_button
  , datePickerConfig_parent
  , datePickerConfig_weekStart
  , datePickerConfig_attributes
  , datePickerConfig_visibleOnLoad
  , datePickerConfig_hideRule
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default
import           Data.Map                    (Map)
import           Data.Maybe
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time
import           GHCJS.DOM.Element
import           Language.Javascript.JSaddle hiding (create)
import           Reflex.Dom.Core             hiding (Element, fromJSString)
import           Reflex.Dom.DHTMLX.Common    (DhtmlxCalendar, WeekDay (..),
                                              calendarConfig_button,
                                              calendarConfig_input,
                                              calendarConfig_parent,
                                              calendarConfig_weekStart,
                                              dateWidgetHide, dateWidgetShow,
                                              hideTime, setPosition,
                                              withCalendar)

------------------------------------------------------------------------------

newtype DateWidgetRef = DateWidgetRef DhtmlxCalendar
  deriving (ToJSVal, MakeObject)


------------------------------------------------------------------------------
getDateWidgetValue :: MonadJSM m => DateWidgetRef -> m Text
getDateWidgetValue a = liftJSM $ valToText =<< a ^. js1 "getDate" True


------------------------------------------------------------------------------
dateWidgetUpdates
    :: (TriggerEvent t m, MonadJSM m)
    => DateWidgetRef
    -> m (Event t Text)
dateWidgetUpdates cal = do
    (event, trigger) <- newTriggerEvent
    void $ liftJSM $ cal ^. js2 "attachEvent" "onClick" (fun $ \_ _ _ -> liftIO . trigger =<< getDateWidgetValue cal)
    return event


------------------------------------------------------------------------------
data DatePickerConfig t = DatePickerConfig
    { _datePickerConfig_initialValue  :: Maybe Day
    , _datePickerConfig_setValue      :: Event t (Maybe Day)
    , _datePickerConfig_button        :: Maybe Element
    , _datePickerConfig_parent        :: Maybe Element
    , _datePickerConfig_weekStart     :: WeekDay
    , _datePickerConfig_attributes    :: Dynamic t (Map Text Text)
    , _datePickerConfig_visibleOnLoad :: Bool
    -- | a function from the date update event to an event that hides the date picker. Defaulted to id.
    , _datePickerConfig_hideRule      :: Event t () -> Event t ()
    }

makeLenses ''DatePickerConfig

instance Reflex t => Default (DatePickerConfig t) where
    def = DatePickerConfig Nothing never Nothing Nothing Sunday mempty False id

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
    :: forall t m. MonadWidget t m
    => DatePickerConfig t
    -> m (DatePicker t)
dhtmlxDatePicker (DatePickerConfig iv sv b p wstart attrs visibleOnLoad hideRule) = do
    let fmt = "%Y-%m-%d"
        formatter = T.pack . maybe "" (formatTime defaultTimeLocale fmt)
        ivTxt = formatter iv
    -- we set the text input with postBuild due to a race condition in dhtmlx-calendar
    pb <- getPostBuild
    ti <- textInput $ def
      & attributes .~ attrs
      & textInputConfig_setValue .~ leftmost [formatter <$> sv, ivTxt <$ pb]
    let dateEl = toElement $ _textInput_element ti
        config = def
          & calendarConfig_button .~ b
          & calendarConfig_parent .~ p
          & calendarConfig_input .~ Just dateEl
          & calendarConfig_weekStart .~ wstart
    ups <- withCalendar config $ \cal -> do
      hideTime cal
      when (isJust p) $ setPosition cal 0 0
      when visibleOnLoad $ dateWidgetShow cal
      ups <- dateWidgetUpdates $ DateWidgetRef cal
      performEvent_ $ dateWidgetHide cal <$ hideRule (() <$ ups)
      return ups
    let parser = parseTimeM True defaultTimeLocale fmt . T.unpack
    fmap DatePicker $ holdDyn iv $ leftmost [parser <$> _textInput_input ti, parser <$> ups, sv]
