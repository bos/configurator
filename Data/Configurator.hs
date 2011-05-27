module Data.Configurator
    (
    ) where

import Data.List
import Control.Applicative
import Control.Monad
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Data.Attoparsec.Text.Lazy
import Data.Configurator.Parser
import Data.Configurator.Types.Internal
import System.IO
import qualified Data.HashMap.Lazy as H
import Data.Maybe
import qualified Data.Text as T

loadFiles :: [Path] -> IO (H.HashMap Path [Directive])
loadFiles = foldM go H.empty
 where
   go seen path = do
     ds <- loadOne (T.unpack path)
     let seen' = H.insert path ds seen
         notKnown n = not . isJust . H.lookup n $ seen
     foldM go seen' . filter notKnown . importsOf $ ds
  
gorb paths = do
  ds <- loadFiles paths
  return (flatten paths ds)

flatten :: [Path] -> H.HashMap Path [Directive] -> H.HashMap Name Value
flatten roots files = foldl' (directive "") H.empty .
                      concat . catMaybes . map (`H.lookup` files) $ roots
 where
  directive prefix m (Bind name value) =
      case value of
        Group xs -> foldl' (directive prefix') m xs
        v        -> H.insert (T.append prefix name) v m
    where prefix' | T.null prefix = name `T.snoc` '.'
                  | otherwise = T.concat [prefix, name, "."]
  directive prefix m (Import path) =
      case H.lookup path files of
        Just ds -> foldl' (flob prefix) m ds
        _       -> m

importsOf :: [Directive] -> [Path]
importsOf (Import path : xs)       = path : importsOf xs
importsOf (Bind _ (Group ys) : xs) = importsOf ys ++ importsOf xs
importsOf (_ : xs)                 = importsOf xs
importsOf _                        = []

loadOne :: FilePath -> IO [Directive]
loadOne path = do
  s <- L.readFile path
  case eitherResult $ parse topLevel s of
    Left err -> hPutStrLn stderr err >> return []
    Right ds -> return ds
