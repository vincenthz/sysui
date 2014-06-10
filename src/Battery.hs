module Battery
    ( percent
    , enumerateBatteries
    , getBatteryInfo
    ) where

import System.Directory
import System.FilePath
import Control.Applicative
import Control.Monad

-- take a number between 0 and 1 and make a number between 0 and 100%
percent :: Double -> Double
percent f = fromIntegral (round (f * 1000) :: Int) / 10

enumerateBatteries = filter (not . flip elem [".",".."]) `fmap` getDirectoryContents powerSupplyDir
-- very simplistic and inefficient way to get NOW / FULL
getBatteryInfo bat = do
    kvs <- (foldl parseAcc [] . lines) `fmap` readFile (powerSupplyDir </> bat </> "uevent")
    case lookup "ONLINE" kvs of
        Just v -> return $ Just $ Left v
        _      -> case sequence $ map (flip lookup kvs) ["CHARGE_FULL","CHARGE_NOW"] of
                      Just [f, n] -> return $ Just $ Right (fromIntegral (read n :: Int) / fromIntegral (read f :: Int))
                      _           -> return Nothing
parseAcc acc s =
    case break (== '=') s of
        (name,'=':val) -> (drop powerSupplyPrefixLen name, val) : acc
        _              -> acc
powerSupplyPrefixLen = 13
powerSupplyDir = "/sys/class/power_supply"
