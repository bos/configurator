{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Data.Configurator.Parser
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- A parser for configuration files.

module Data.Configurator.Parser
    (
      topLevel
    , interp
    ) where

import Control.Applicative
import Control.Exception (throw)
import Control.Monad (when)
import Data.Attoparsec.Text as A
import Data.Bits (shiftL)
import Data.Char (chr, isAlpha, isAlphaNum, isSpace)
import Data.Configurator.Types.Internal
import Data.Monoid (Monoid(..))
import Data.Text (Text)
import Data.Text.Lazy.Builder (fromText, singleton, toLazyText)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L

topLevel :: Parser [Directive]
topLevel = directives <* skipLWS <* endOfInput
  
directive :: Parser Directive
directive =
  mconcat [
    string "import" *> skipLWS *> (Import <$> string_)
  , Bind <$> try (ident <* skipLWS <* char '=' <* skipLWS) <*> value
  , Group <$> try (ident <* skipLWS <* char '{' <* skipLWS)
          <*> directives <* skipLWS <* char '}'
  ]

directives :: Parser [Directive]
directives = (skipLWS *> directive <* skipHWS) `sepBy`
             (satisfy $ \c -> c == '\r' || c == '\n')

data Skip = Space | Comment

-- | Skip lines, comments, or horizontal white space.
skipLWS :: Parser ()
skipLWS = scan Space go *> pure ()
  where go Space c | isSpace c = Just Space
        go Space '#'           = Just Comment
        go Space _             = Nothing
        go Comment '\r'        = Just Space
        go Comment '\n'        = Just Space
        go Comment _           = Just Comment

-- | Skip comments or horizontal white space.
skipHWS :: Parser ()
skipHWS = scan Space go *> pure ()
  where go Space ' '           = Just Space
        go Space '\t'          = Just Space
        go Space '#'           = Just Comment
        go Space _             = Nothing
        go Comment '\r'        = Nothing
        go Comment '\n'        = Nothing
        go Comment _           = Just Comment

ident :: Parser Name
ident = do
  n <- T.cons <$> satisfy isAlpha <*> A.takeWhile isCont
  when (n == "import") $
    throw (ParseError "" $ "reserved word (" ++ show n ++ ") used as identifier")
  return n
 where
  isCont c = isAlphaNum c || c == '_' || c == '-' || c == ':'

value :: Parser Value
value = mconcat [
          string "on" *> pure (Bool True)
        , string "off" *> pure (Bool False)
        , string "true" *> pure (Bool True)
        , string "false" *> pure (Bool False)
        , String <$> string_
        , Number <$> rational
        , List <$> brackets '[' ']'
                   ((value <* skipLWS) `sepBy` (char ',' <* skipLWS))
        ]

string_ :: Parser Text
string_ = do
  s <- char '"' *> scan False isChar <* char '"'
  if "\\" `T.isInfixOf` s
    then unescape s
    else return s
 where
  isChar True _ = Just False
  isChar _ '"'  = Nothing
  isChar _ c    = Just (c == '\\')

brackets :: Char -> Char -> Parser a -> Parser a
brackets open close p = char open *> skipLWS *> p <* char close

embed :: Parser a -> Text -> Parser a
embed p s = case parseOnly p s of
              Left err -> fail err
              Right v  -> return v

unescape :: Text -> Parser Text
unescape = fmap (L.toStrict . toLazyText) . embed (p mempty)
 where
  p acc = do
    h <- A.takeWhile (/='\\')
    let rest = do
          let cont c = p (acc `mappend` fromText h `mappend` singleton c)
          c <- char '\\' *> satisfy (inClass "ntru\"\\")
          case c of
            'n'  -> cont '\n'
            't'  -> cont '\t'
            'r'  -> cont '\r'
            '"'  -> cont '"'
            '\\' -> cont '\\'
            _    -> cont =<< hexQuad
    done <- atEnd
    if done
      then return (acc `mappend` fromText h)
      else rest

hexQuad :: Parser Char
hexQuad = do
  a <- embed hexadecimal =<< A.take 4
  if a < 0xd800 || a > 0xdfff
    then return (chr a)
    else do
      b <- embed hexadecimal =<< string "\\u" *> A.take 4
      if a <= 0xdbff && b >= 0xdc00 && b <= 0xdfff
        then return $! chr (((a - 0xd800) `shiftL` 10) + (b - 0xdc00) + 0x10000)
        else fail "invalid UTF-16 surrogates"
                   
-- | Parse a string interpolation spec.
--
-- The sequence @$$@ is treated as a single @$@ character.  The
-- sequence @$(@ begins a section to be interpolated, and @)@ ends it.
interp :: Parser [Interpolate]
interp = reverse <$> p []
 where
  p acc = do
    h <- Literal <$> A.takeWhile (/='$')
    let rest = do
          let cont x = p (x : h : acc)
          c <- char '$' *> satisfy (\c -> c == '$' || c == '(')
          case c of
            '$' -> cont (Literal (T.singleton '$'))
            _   -> (cont . Interpolate) =<< A.takeWhile1 (/=')') <* char ')'
    done <- atEnd
    if done
      then return (h : acc)
      else rest
