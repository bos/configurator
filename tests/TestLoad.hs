{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.Configurator
import System.Environment

main = do
  args <- getArgs
  putStrLn $ "files: " ++ show args
  e <- try $ load args
  case e of
    Left (err::SomeException) -> putStrLn $ "error: " ++ show err
    Right c -> putStr "ok: " >> display c
