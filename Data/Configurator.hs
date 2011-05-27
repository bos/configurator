{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Data.Configurator
    (
      load
    ) where

import Data.List
import Control.Exception
import Control.Applicative
import Data.Monoid
import Control.Monad
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder (fromString, fromText, toLazyText)
import qualified Data.Attoparsec.Text.Lazy as L
import qualified Data.Attoparsec.Text as T
import System.Environment (getEnv)
import Data.Configurator.Parser
import Data.Configurator.Types.Internal
import System.IO
import qualified Data.HashMap.Lazy as H
import Data.Maybe
import Prelude hiding (catch)
import qualified Data.Text as T

loadFiles :: [Path] -> IO (H.HashMap Path [Directive])
loadFiles = foldM go H.empty
 where
   go seen path = do
     ds <- loadOne (T.unpack path)
     let seen' = H.insert path ds seen
         notSeen n = not . isJust . H.lookup n $ seen
     foldM go seen' . filter notSeen . importsOf $ ds
  
load :: [Path] -> IO (H.HashMap Name Value)
load paths = do
  ds <- loadFiles paths
  return (flatten paths ds)

flatten :: [Path] -> H.HashMap Path [Directive] -> H.HashMap Name Value
flatten roots files = foldl' (directive "") H.empty .
                      concat . catMaybes . map (`H.lookup` files) $ roots
 where
  directive prefix m (Bind name value) = H.insert (T.append prefix name) value m
  directive prefix m (Group name xs) = foldl' (directive prefix') m xs
    where prefix' = T.concat [prefix, name, "."]
  directive prefix m (Import path) =
      case H.lookup path files of
        Just ds -> foldl' (directive prefix) m ds
        _       -> m

interpolate :: T.Text -> H.HashMap Name Value -> IO T.Text
interpolate s env
    | "$(" `T.isInfixOf` s =
      case T.parseOnly interp s of
        Left _   -> undefined
        Right xs -> (L.toStrict . toLazyText . mconcat) <$> mapM interpret xs
    | otherwise = return s
 where
  interpret (Literal x)   = return (fromText x)
  interpret (Interp name) =
      case H.lookup name env of
        Just (String x) -> return (fromText x)
        Just (Number n) -> return (decimal n)
        Just _          -> error "type error"
        _ -> do
          e <- try . getEnv . T.unpack $ name
          case e of
            Left (_::SomeException) -> error "no such variable"
            Right x -> return (fromString x)

importsOf :: [Directive] -> [Path]
importsOf (Import path : xs) = path : importsOf xs
importsOf (Group _ ys : xs)  = importsOf ys ++ importsOf xs
importsOf (_ : xs)           = importsOf xs
importsOf _                  = []

loadOne :: FilePath -> IO [Directive]
loadOne path = do
  s <- L.readFile path
  p <- evaluate (L.eitherResult $ L.parse topLevel s)
       `catch` \(e::ConfigError) ->
       throwIO $ case e of
                   ParseError _ err -> ParseError path err
  case p of
    Left err -> throwIO (ParseError path err)
    Right ds -> return ds
