module Graphics.Sysui.PowerSupply
    ( -- * PowerSupplies
      PowerSupplies(..)
    , getPowerSupplies
    , takeDeviceName
      -- * Device
      -- ** Battery
    , BatteryInfos(..)
    , BatteryStatus(..)
    , getBatteryInfos
      -- ** AC
    , isACOnline
    ) where

import Graphics.Sysui.PowerSupply.Battery
import Graphics.Sysui.PowerSupply.AC

import qualified System.IO.Unsafe as X (unsafePerformIO)
import System.Directory (getDirectoryContents)
import System.FilePath  (takeFileName, (</>))

-- Linux power supply common path:
powerSupplyDir = "/sys/class/power_supply"

-- | Lists of Power Supplies sorted by type
data PowerSupplies = PowerSupplies
    { batteries :: [FilePath]
    , acwires   :: [FilePath]
    , unknowns  :: [FilePath] }
    deriving (Show)

takeDeviceName :: FilePath -> FilePath
takeDeviceName = takeFileName

getPowerSupplies :: IO PowerSupplies
getPowerSupplies = do
    dirs <- enumeratePowerSupply
    let (bats, acs, others) = foldr selectPowerSupply ([],[],[]) dirs
    return $ PowerSupplies bats acs others
    where
        selectPowerSupply :: FilePath
                          -> ([FilePath], [FilePath], [FilePath])
                          -> ([FilePath], [FilePath], [FilePath])
        selectPowerSupply d (bl, acl, ol) =
            let deviceDir = powerSupplyDir </> d
                t = X.unsafePerformIO $ readFile $ deviceDir </> "type"
            in  case t of
                    "Mains\n"   -> (bl,           deviceDir:acl, ol)
                    "Battery\n" -> (deviceDir:bl, acl,           ol)
                    _           -> (bl,           acl,           deviceDir:ol)
        enumeratePowerSupply :: IO [FilePath]
        enumeratePowerSupply = filter (not . flip elem [".",".."]) `fmap` getDirectoryContents powerSupplyDir
