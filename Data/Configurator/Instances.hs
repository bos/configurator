{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Configurator.Instances () where

import Control.Applicative
import Data.Configurator.Types.Internal
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Lazy as L

instance Configured Value where
    convert = Just

instance Configured Bool where
    convert (Bool v) = Just v
    convert _        = Nothing

convertNumber :: (Num a) => Value -> Maybe a
convertNumber (Number v) = Just (fromIntegral v)
convertNumber _          = Nothing

instance Configured Int where
    convert = convertNumber

instance Configured Integer where
    convert = convertNumber

instance Configured Int8 where
    convert = convertNumber

instance Configured Int16 where
    convert = convertNumber

instance Configured Int32 where
    convert = convertNumber

instance Configured Int64 where
    convert = convertNumber

instance Configured Word where
    convert = convertNumber

instance Configured Word8 where
    convert = convertNumber

instance Configured Word16 where
    convert = convertNumber

instance Configured Word32 where
    convert = convertNumber

instance Configured Word64 where
    convert = convertNumber

instance Configured T.Text where
    convert (String v) = Just v
    convert _          = Nothing

instance Configured [Char] where
    convert = fmap T.unpack . convert

instance Configured L.Text where
    convert = fmap L.fromStrict . convert

instance Configured B.ByteString where
    convert = fmap encodeUtf8 . convert

instance Configured LB.ByteString where
    convert = fmap (LB.fromChunks . (:[])) . convert

instance (Configured a, Configured b) => Configured (a,b) where
    convert (List [a,b]) = (,) <$> convert a <*> convert b
    convert _            = Nothing

instance (Configured a, Configured b, Configured c) => Configured (a,b,c) where
    convert (List [a,b,c]) = (,,) <$> convert a <*> convert b <*> convert c
    convert _              = Nothing

instance (Configured a, Configured b, Configured c, Configured d)
    => Configured (a,b,c,d) where
    convert (List [a,b,c,d]) = (,,,) <$> convert a <*> convert b <*> convert c
                                     <*> convert d
    convert _                = Nothing
