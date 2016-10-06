{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Configurator.Instances () where

import Control.Applicative
import Data.Configurator.Types.Internal
import Data.Complex (Complex)
import Data.Fixed (Fixed, HasResolution)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text.Encoding (encodeUtf8)
import Data.Ratio (Ratio, denominator, numerator)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Foreign.C.Types (CDouble, CFloat)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Lazy as L

instance Configured Value where
    convert = Just

instance Configured Bool where
    convert (Bool v) = Just v
    convert _        = Nothing

convertNumberToNum :: (Num a) => Value -> Maybe a
convertNumberToNum (Number r)
    | denominator r == 1 = Just $ fromInteger $ numerator r
convertNumberToNum _ = Nothing

instance Configured Int where
    convert = convertNumberToNum

instance Configured Integer where
    convert = convertNumberToNum

instance Configured Int8 where
    convert = convertNumberToNum

instance Configured Int16 where
    convert = convertNumberToNum

instance Configured Int32 where
    convert = convertNumberToNum

instance Configured Int64 where
    convert = convertNumberToNum

instance Configured Word where
    convert = convertNumberToNum

instance Configured Word8 where
    convert = convertNumberToNum

instance Configured Word16 where
    convert = convertNumberToNum

instance Configured Word32 where
    convert = convertNumberToNum

instance Configured Word64 where
    convert = convertNumberToNum

convertNumberToFractional :: (Fractional a) => Value -> Maybe a
convertNumberToFractional (Number r) = Just $ fromRational r
convertNumberToFractional _ = Nothing

instance Configured Double where
    convert = convertNumberToFractional

instance Configured Float where
    convert = convertNumberToFractional

instance Configured CDouble where
    convert = convertNumberToFractional

instance Configured CFloat where
    convert = convertNumberToFractional

instance Integral a => Configured (Ratio a) where
    convert = convertNumberToFractional

instance RealFloat a => Configured (Complex a) where
    convert = convertNumberToFractional

instance HasResolution a => Configured (Fixed a) where
    convert = convertNumberToFractional

instance Configured T.Text where
    convert (String v) = Just v
    convert _          = Nothing

instance Configured Char where
    convert (String txt) | T.length txt == 1 = Just $ T.head txt
    convert _ = Nothing

    convertList = fmap T.unpack . convert

instance Configured L.Text where
    convert = fmap L.fromStrict . convert

instance Configured B.ByteString where
    convert = fmap encodeUtf8 . convert

instance Configured LB.ByteString where
    convert = fmap (LB.fromChunks . (:[])) . convert

instance Configured a => Configured [a] where
    convert (List xs) = mapM convert xs
    convert _         = Nothing

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
