{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.IORef
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Hourglass
import System.Hourglass
import Data.Either
import Data.Maybe
import System.FilePath ((</>))
import System.Directory
import System.Posix.IO (stdInput, fdReadBuf)
import Graphics.UI.Gtk
import qualified Data.ByteString.Internal as B (createAndTrim)
import Data.ByteString.UTF8 (toString)

import Battery

debug = False

withSection box name f = do
    lbl <- labelNew (Just name)
    boxPackStart box lbl PackNatural 2

    (widget, a) <- f
    boxPackStart box widget PackNatural 2
    return a

timeUnit = 500 -- in milliseconds
seconds v = v * (1000 `div` timeUnit)
batteryUpdateTime = seconds 30

getCurrentFormattedTime tz = do
    let pdt = TimezoneOffset (-7*60)
    e <- localTimeFromGlobal <$> timeCurrent
    let localMy = localTimeSetTimezone tz e
        localPdt = localTimeSetTimezone pdt e
        fmt = "YYYY-MM-DD H:MI:S" :: String
    return (localTimePrint fmt localMy, localTimePrint fmt localPdt)

main = do
    ctrRef <- newIORef 0
    _    <- initGUI

    tz <- timezoneCurrent
    Just disp <- displayGetDefault
    nScreens  <- displayGetNScreens disp
    --screen    <- displayGetScreen disp 0
    --nMonitors <- screenGetNMonitors 0
    --monitorSize <- screenGetMonitorGeometry screen onMonitor

    when debug $ do
        putStrLn ("screens : " ++ show nScreens)
        --putStrLn ("monitors: " ++ show nMonitors)
        --putStrLn ("monitor: "  ++ show monitorSize)

    --let Rectangle x y w _ = monitorSize

    window <- windowNew
    widgetSetName window "sysui"

    box <- vBoxNew False 15
    containerAdd window box

    clockUpdate <- withSection box "Clock" $ do
        clockBox <- vBoxNew False 15
        (iniL, iniPdt) <- getCurrentFormattedTime tz
        lblLocal <- labelNew (Just ("local time: " ++ iniL))
        lblPdt   <- labelNew (Just ("PDT time: " ++ iniPdt))
        boxPackStart clockBox lblLocal PackNatural 2
        boxPackStart clockBox lblPdt PackNatural 2
        let setter = getCurrentFormattedTime tz >>= \(l, pst) -> do
                        labelSetText lblLocal ("local time: " ++ l)
                        labelSetText lblPdt ("PDT time: " ++ pst)
        return (clockBox, setter)

    batteryUpdate <- withSection box "Battery" $ do
        batteryBox <- vBoxNew False 15

        batWidgets <- enumerateBatteries >>= mapM (\bat -> do
                        h   <- hBoxNew False 20
                        lbl <- labelNew (Just "")
                        lb  <- levelBarNew

                        set lb [ levelBarMinValue := 0.0, levelBarMaxValue := 100.0 ]

                        boxPackStart h lbl PackNatural 2
                        boxPackStart h lb PackNatural 2
                        boxPackStart batteryBox h PackNatural 2
                        return (bat, (lbl, lb)))

        let update = do
                batNames <- enumerateBatteries
                forM_ batNames $ \bat -> do
                    info <- getBatteryInfo bat
                    let (txt, v) = case info of
                                Just (Left bv)  -> (if bv == "1" then "ONLINE" else "OFFLINE", -1)
                                Just (Right p) -> (show (percent p) ++ "%", percent p)
                                Nothing        -> ("ERROR", -1)

                    case lookup bat batWidgets of
                        Nothing        -> return ()
                        Just (lblWidget, lb) -> do
                            labelSetText lblWidget ("bat: " ++ txt)
                            if (not (v < 0.0))
                                then set lb [ levelBarValue := v ]
                                else return ()
        update
        return (batteryBox, update)

    withSection box "Calendar" $ do
        c <- calendarNew
        return (c, ())

    flip timeoutAdd timeUnit $ do
        ctr <- readIORef ctrRef
        when ((ctr `mod` batteryUpdateTime) == 0) batteryUpdate

        -- update date every now and then
        clockUpdate

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
