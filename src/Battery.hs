module Battery
    ( PowerSupply(..)
    , BatteryStatus(..)
    , enumerateBatteries
    , getBatteryStatus
    , isACOnline
    ) where

import System.Directory
import System.FilePath

import System.IO.Unsafe

-- Linux power supply common path:
powerSupplyDir = "/sys/class/power_supply"

data BatteryStatus = BSFull
                   | BSDischarging
                   | BSCharging
                   | BSUnknown

data PowerSupply = PowerSupply
    { batteries :: [FilePath]
    , acwires   :: [FilePath]
    , unknowns  :: [FilePath] }
    deriving (Show)

-- take a number between 0 and 1 and make a number between 0 and 100%
percent :: Double -> Double
percent f = fromIntegral (round (f * 1000) :: Int) / 10

-- get battery status
getBatteryStatus :: FilePath -> IO (BatteryStatus)
getBatteryStatus bat = do
    st <- readFile $ powerSupplyDir </> bat </> "status"
    return $ case st of
                "Full\n"        -> BSFull
                "Discharging\n" -> BSDischarging
                "Charging\n"    -> BSCharging
                _               -> BSUnknown

isACOnline :: FilePath -> IO Bool
isACOnline ac = readFile (powerSupplyDir </> ac </> "online") >>= \online -> return $ online == "1\n"

enumerateBatteries :: IO PowerSupply
enumerateBatteries = do
    dirs <- enumerateBatteries_
    let (bats, acs, others) = foldr selectBattery ([],[],[]) dirs
    return $ PowerSupply bats acs others
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
