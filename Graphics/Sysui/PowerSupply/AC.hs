module Graphics.Sysui.PowerSupply.AC
    ( isACOnline
    ) where

import System.FilePath (FilePath, (</>))

isACOnline :: FilePath -> IO Bool
isACOnline ac = readFile (ac </> "online") >>= \online -> return $ online == "1\n"
