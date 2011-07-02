{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Control.Concurrent
import Data.Configurator
import Data.Configurator.Types
import qualified Data.ByteString.Lazy.Char8 as L
import System.Environment
import System.Directory
import Control.Monad
import System.IO

main = do
  args <- getArgs
  tmpDir <- getTemporaryDirectory
  temps <- forM args $ \arg -> do
           (p,h) <- openBinaryTempFile tmpDir "test.cfg"
           L.hPut h =<< L.readFile arg
           hClose h
           return p
  flip finally (mapM_ removeFile temps) $ do
    done <- newEmptyMVar
    let myConfig = autoConfig {
                     onError = \e -> hPutStrLn stderr $ "uh oh: " ++ show e
                   }
    (c,_) <- autoReload myConfig (map Required temps)
    display c
    subscribe c "dongly" $ \n v -> print (n,v) >> putMVar done ()
    forM_ temps $ \t -> L.appendFile t "\ndongly = 1\n"
    takeMVar done
