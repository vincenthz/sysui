{-# LANGUAGE OverloadedStrings #-}
module Graphics.Sysui
    ( -- * Configure Sysui
      SysuiConfig(..)
    , TimeToShow(..)
      -- * main function
    , defaultMain
      -- * Default Configuration Options
    , defaultTimeFormat
    , defaultTimeToShow
    , defaultBatteryRefreshingTimer
    , defaultSysuiConfig
    )where

import Data.IORef
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Hourglass
import System.Hourglass
import Graphics.UI.Gtk

import Graphics.Sysui.PowerSupply

-- Configuration interface:

timeUnit = 500 -- in milliseconds
seconds v = v * (1000 `div` timeUnit)

defaultTimeFormat :: String
defaultTimeFormat = "YYYY-MM-DD H:MI:S"

defaultTimeToShow :: IO TimeToShow
defaultTimeToShow = timezoneCurrent >>= \tz -> return $ TimeToShow "Local" tz

defaultBatteryRefreshingTimer :: Int
defaultBatteryRefreshingTimer = seconds 30

data TimeToShow = TimeToShow
    { getTSLabel_  :: String
    , getTSOffset_ :: TimezoneOffset
    }

data SysuiConfig = SysuiConfig
    { timeFormat :: String
    , timeToShow :: [TimeToShow]
    , showCalendar :: Bool
    , batteryRefreshingTime :: Int
    }

defaultSysuiConfig :: IO SysuiConfig
defaultSysuiConfig =
    defaultTimeToShow >>= \currentTZ ->
        return $ SysuiConfig
                    defaultTimeFormat -- set the default time format
                    [ currentTZ ]     -- set the default list of clock to show
                    True              -- need the calendar
                    defaultBatteryRefreshingTimer

-- Battery

createBatteryWidget box bat = do
    h   <- hBoxNew False 20
    lbl <- labelNew (Just "")
    lb  <- levelBarNew

    set lb [ levelBarMinValue := 0.0, levelBarMaxValue := 100.0 ]

    boxPackStart h lbl PackNatural 2 
    boxPackStart h lb PackNatural 2
    boxPackStart box h PackNatural 2
    return (bat, (lbl, lb))

updateBatteryWidget wl bat = do
    infos <- getBatteryInfos bat
    let (txt, lvl) = case getStatus_ infos of
                        BSFull        -> ( "Full", Just 100)
                        BSDischarging -> ( "Discharging (" ++ (showCapacity infos) ++ " | " ++ (showDuration infos) ++ " left )"
                                         , getCapacity_ infos )
                        BSCharging    -> ( "Charging (" ++ (showCapacity infos) ++ ")"
                                         , getCapacity_ infos )
                        _             -> ( "Error", Nothing )
    case lookup bat wl of
        Nothing             -> return ()
        Just (lblWidget, lb) -> do
            labelSetText lblWidget ((takeDeviceName bat) ++ ": " ++ txt)
            set lb [ levelBarValue := (maybe 0.0 fromIntegral lvl) ]
    where
        showDuration :: BatteryInfos -> String
        showDuration bi =
            let showDuration_ d = (show $ durationHours d) ++ (show $ durationMinutes d) ++ (show $ durationSeconds d)
            in  maybe "N/A" showDuration_ $ getDuration_ bi

        showCapacity :: BatteryInfos -> String
        showCapacity bi =
            let showCapacity_ c = (show c) ++ "%"
            in  maybe "N/A" showCapacity_ $ getCapacity_ bi

-- AC

createACWidget box ac = do
    h   <- hBoxNew False 20
    lbl <- labelNew (Just "")


    boxPackStart h lbl PackNatural 2 
    boxPackStart box h PackNatural 2
    return (ac, lbl)

updateACWidget wl ac = do
    st <- isACOnline ac
    let status = if st then "ONLINE" else "OFFLINE"
    case lookup ac wl of
        Nothing        -> return ()
        Just lblWidget -> labelSetText lblWidget ((takeDeviceName ac) ++ ": " ++ status)

-- Clock

createClockWidget box tts = do
    lbl <- labelNew (Nothing)
    boxPackStart box lbl PackNatural 2 
    return (getTSLabel_ tts, lbl) 

updateClockWidget e fmt wl (TimeToShow label offset) =
    let t    = localTimeSetTimezone offset e
        fmtT = localTimePrint fmt t
    in  case lookup label wl of
            Nothing -> return ()
            Just w -> labelSetText w (label ++ ": " ++ fmtT)

-- Graphics

-- withSection :: (BoxClass b, WidgetClass w) => b -> String -> IO (w, y) -> y
withSection box name f = do
    lbl <- labelNew (Just name)
    boxPackStart box lbl PackNatural 2

    (widget, a) <- f
    boxPackStart box widget PackNatural 2
    return a

createPowerSupplyWidget :: BoxClass b => b -> SysuiConfig -> IO (IO ())
createPowerSupplyWidget box _ =
    withSection box "Power Supply" $ do
        powerBox <- vBoxNew True 15

        (PowerSupplies batList acList _) <- getPowerSupplies

        batteriesUpdate <- withSection powerBox "Batteries" $ do
            batteryBox <- vBoxNew False 15
            batWidgets <- mapM (createBatteryWidget batteryBox) batList
            let updateBatteries = forM_ batList (updateBatteryWidget batWidgets)
            return (batteryBox, updateBatteries)
        acUpdate <- withSection powerBox "Wires" $ do
            acBox <- vBoxNew False 15
            acWidgets <- mapM (createACWidget acBox) acList
            let updateACs = forM_ acList (updateACWidget acWidgets)
            return (acBox, updateACs)

        -- TODO: check if a battery has been added or deleted and create/delete the widget in case
        let updatePower = sequence_ [batteriesUpdate, acUpdate]
        updatePower
        return (powerBox, updatePower)

createTimeWidget :: BoxClass b => b -> SysuiConfig -> IO (IO ())
createTimeWidget box config
    | not $ null $ timeToShow config =
        withSection box "Clock" $ do
            clocksBox <- vBoxNew False 15
            clockWidgets <- mapM (createClockWidget clocksBox) (timeToShow config)
            let updateClocks = (localTimeFromGlobal <$> timeCurrent) >>= \refLocal ->
                                    forM_ (timeToShow config) (updateClockWidget refLocal (timeFormat config) clockWidgets)
            updateClocks
            return (clocksBox, updateClocks)
    | otherwise = return $ return ()

createCalendarWidget :: BoxClass b => b -> SysuiConfig -> IO ()
createCalendarWidget box config
    | (showCalendar config) == True =
        withSection box "Calendar" $ do
            c <- calendarNew
            return (c, ())
    | otherwise = return ()

-- Default main:
defaultMain :: SysuiConfig -> IO ()
defaultMain sysuiConfig = do
    ctrRef <- newIORef 0
    _ <- initGUI

    -- Create the main application's window
    window <- windowNew
    widgetSetName window "sysui"

    -- Create the main applications's box
    box <- vBoxNew False 15
    containerAdd window box

    -- generate the needed widgets
    powerUpdate <- createPowerSupplyWidget box sysuiConfig
    timeUpdate <- createTimeWidget box sysuiConfig
    _ <- createCalendarWidget box sysuiConfig

    -- Timer to update the different widget which need to be updated periodicaly
    flip timeoutAdd timeUnit $ do
        ctr <- readIORef ctrRef
        when ((ctr `mod` (batteryRefreshingTime sysuiConfig)) == 0) powerUpdate

        -- update date every now and then
        timeUpdate

        writeIORef ctrRef (ctr+1)
        return True

    window `on` keyPressEvent $ do
        em <- eventModifier
        name <- eventKeyName
        case (em,name) of
            ([Control], "q") -> liftIO mainQuit >> return True
            ([], "Return")   -> liftIO mainQuit >> return True
            _                -> return False

    widgetShowAll window

    mainGUI
