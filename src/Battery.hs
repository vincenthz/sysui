module Battery
    ( -- * PowerSupplies
      PowerSupplies(..)
    , enumerateBatteries
      -- * Battery informations
    , BatteryInfos(..)
    , BatteryStatus(..)
    , getBatteryInfos
      -- * AC informations
    , isACOnline
    ) where

import System.Directory
import System.FilePath

import System.IO.Unsafe
import Data.Hourglass

-- Linux power supply common path:
powerSupplyDir = "/sys/class/power_supply"

-- | Lists of Power Supplies sorted by type
data PowerSupplies = PowerSupplies
    { batteries :: [FilePath]
    , acwires   :: [FilePath]
    , unknowns  :: [FilePath] }
    deriving (Show)

enumerateBatteries :: IO PowerSupplies
enumerateBatteries = do
    dirs <- enumerateBatteries_
    let (bats, acs, others) = foldr selectBattery ([],[],[]) dirs
    return $ PowerSupplies bats acs others
    where
        selectBattery :: FilePath
                      -> ([FilePath], [FilePath], [FilePath])
                      -> ([FilePath], [FilePath], [FilePath])
        selectBattery d (bl, acl, ol) = do
            let t = unsafePerformIO $ readFile (powerSupplyDir </> d </> "type")
            case t of
                "Mains\n"   -> (bl, d:acl, ol)
                "Battery\n" -> (d:bl, acl, ol)
                _           -> (bl, acl, d:ol)
        enumerateBatteries_ = filter (not . flip elem [".",".."]) `fmap` getDirectoryContents powerSupplyDir

-- ###########################################################################
-- Extract information from power supplies depending on their type

-- ## Get Battery informations

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
    uevent <- (foldl parseAcc [] . lines) `fmap` (readFile $ powerSupplyDir </> bat </> "uevent")
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

-- ## get AC informations

isACOnline :: FilePath -> IO Bool
isACOnline ac = readFile (powerSupplyDir </> ac </> "online") >>= \online -> return $ online == "1\n"
