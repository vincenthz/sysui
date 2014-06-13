module Graphics.Sysui.PowerSupply.Battery
    ( BatteryInfos(..)
    , BatteryStatus(..)
    , getBatteryInfos
    ) where

import System.FilePath (FilePath, (</>))
import Data.Hourglass

data BatteryStatus = BSFull
                   | BSDischarging
                   | BSCharging
                   | BSUnknown
    deriving (Show, Eq)

data BatteryInfos = BatteryInfos
    { isPresent_   :: Bool
    , getStatus_   :: BatteryStatus
    , getCapacity_ :: Maybe Int
    , getDuration_ :: Maybe Duration
    }

-- | return battery informations
getBatteryInfos :: FilePath -> IO BatteryInfos
getBatteryInfos bat = do
    uevent <- (foldl parseAcc [] . lines) `fmap` (readFile $ bat </> "uevent")
    return $ BatteryInfos
                (getBatteryPresent uevent)
                (getBatteryStatus uevent)
                (getBatteryChargeRemaining uevent)
                (getBatteryChargeTimeToEmpty uevent)
    where
        parseAcc :: [(String, String)] -> String -> [(String, String)]
        parseAcc acc s =
            case break (== '=') s of
                (name,'=':val) -> (drop powerSupplyPrefixLen name, val) : acc
                _              -> acc
        powerSupplyPrefixLen = 13

getBatteryChargeRemaining :: [(String, String)] -> Maybe Int
getBatteryChargeRemaining l =
    case lookup "CAPACITY" l of
        Just c  -> Just $ read c
        Nothing -> Nothing

getBatteryChargeTimeToEmpty :: [(String, String)] -> Maybe Duration
getBatteryChargeTimeToEmpty l =
    case sequence $ map (flip lookup l) ["CHARGE_NOW","CURRENT_NOW"] of
        Just [chNow, cuNow] -> let chFI = fromIntegral (read chNow :: Int) :: Double
                                   cuFI = fromIntegral (read cuNow :: Int) :: Double
                                   seconds = Seconds $ round $ 3600.0 * chFI / cuFI
                                   (d, _) = fromSeconds seconds
                               in  Just d
        _                   -> Nothing

getBatteryStatus :: [(String, String)] -> BatteryStatus
getBatteryStatus l =
    case lookup "STATUS" l of
        Just "Full"        -> BSFull
        Just "Discharging" -> BSDischarging
        Just "Charging"    -> BSCharging
        _                  -> BSUnknown
getBatteryPresent :: [(String, String)] -> Bool
getBatteryPresent l =
    case lookup "PRESENT" l of
        Just "1" -> True
        _        -> False
