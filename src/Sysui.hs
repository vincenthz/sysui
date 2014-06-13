{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Hourglass

import Graphics.Sysui

mySysuiConfig :: IO SysuiConfig
mySysuiConfig = do
    defaultTTS <- defaultTimeToShow
    return $ SysuiConfig
                defaultTimeFormat
                [defaultTTS, TimeToShow "UTC" timezone_UTC]
                False
                defaultBatteryRefreshingTimer

-- Run the default main with the default configuration file
main = mySysuiConfig >>= defaultMain

