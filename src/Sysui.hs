{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Hourglass
import System.Process (readProcessWithExitCode)

import Graphics.Sysui

mySysuiConfig :: IO SysuiConfig
mySysuiConfig = do
    currentTTS <- defaultTimeToShow
    (_, t, _) <- readProcessWithExitCode "date" ["+%Z"] ""
    return $ SysuiConfig
                defaultTimeFormat
                (listOfTimeToShow currentTTS t)
                False
                defaultBatteryRefreshingTimer
    where
        listOfTimeToShow currentTTS t =
            [ currentTTS
            , TimeToShow "UTC" timezone_UTC
            , TimeToShow "France" $ TimezoneOffset $ parisTZOffset t
            ]
        parisTZOffset t =
            case t of
                "BST\n" -> 2 * 60
                "GMT\n" -> 1 * 60
                _       -> 1 * 60


-- Run the default main with the default configuration file
main = mySysuiConfig >>= defaultMain

