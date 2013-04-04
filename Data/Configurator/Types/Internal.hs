{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}

-- |
-- Module:      Data.Configurator.Types.Internal
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with configuration files.

module Data.Configurator.Types.Internal
    (
      BaseConfig(..)
    , Config(..)
    , Configured(..)
    , AutoConfig(..)
    , Worth(..)
    , Name
    , Value(..)
    , Binding
    , Path
    , Directive(..)
    , ConfigError(..)
    , KeyError(..)
    , Interpolate(..)
    , Pattern(..)
    , exact
    , prefix
    , ChangeHandler
    ) where

import Control.Exception
import Data.Data (Data)
import Data.Hashable (Hashable(..))
import Data.IORef (IORef)
import Data.List (isSuffixOf)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Prelude hiding (lookup)
import qualified Data.HashMap.Lazy as H

data Worth a = Required { worth :: a }
             | Optional { worth :: a }
               deriving (Show, Typeable)

instance IsString (Worth FilePath) where
    fromString = Required

instance (Eq a) => Eq (Worth a) where
    a == b = worth a == worth b

instance (Hashable a) => Hashable (Worth a) where
    hashWithSalt salt v = hashWithSalt salt (worth v)

-- | Global configuration data.  This is the top-level config from which
-- 'Config' values are derived by choosing a root location.
data BaseConfig = BaseConfig {
      cfgAuto :: Maybe AutoConfig
    , cfgPaths :: IORef [(Name, Worth Path)]
    -- ^ The files from which the 'Config' was loaded.
    , cfgMap :: IORef (H.HashMap Name Value)
    , cfgSubs :: IORef (H.HashMap Pattern [ChangeHandler])
    }

-- | Configuration data.
data Config = Config { root :: Text, baseCfg :: BaseConfig }

instance Functor Worth where
    fmap f (Required a) = Required (f a)
    fmap f (Optional a) = Optional (f a)

-- | An action to be invoked if a configuration property is changed.
--
-- If this action is invoked and throws an exception, the 'onError'
-- function will be called.
type ChangeHandler = Name
                   -- ^ Name of the changed property.
                   -> Maybe Value
                   -- ^ Its new value, or 'Nothing' if it has
                   -- vanished.
                   -> IO ()

-- | A pattern specifying the name of a property that has changed.
--
-- This type is an instance of the 'IsString' class.  If you use the
-- @OverloadedStrings@ language extension and want to write a
-- 'prefix'-matching pattern as a literal string, do so by suffixing
-- it with \"@.*@\", for example as follows:
--
-- > "foo.*"
--
-- If a pattern written as a literal string does not end with
-- \"@.*@\", it is assumed to be 'exact'.
data Pattern = Exact Name
             -- ^ An exact match.
             | Prefix Name
             -- ^ A prefix match.  Given @'Prefix' \"foo\"@, this will
             -- match @\"foo.bar\"@, but not @\"foo\"@ or
             -- @\"foobar\"@.
               deriving (Eq, Show, Typeable, Data)

-- | A pattern that must match exactly.
exact :: Text -> Pattern
exact = Exact

-- | A pattern that matches on a prefix of a property name.  Given
-- @\"foo\"@, this will match @\"foo.bar\"@, but not @\"foo\"@ or
-- @\"foobar\"@.
prefix :: Text -> Pattern
prefix p = Prefix (p `T.snoc` '.')

instance IsString Pattern where
    fromString s
        | ".*" `isSuffixOf` s = Prefix . T.init . T.pack $ s
        | otherwise           = Exact (T.pack s)

instance Hashable Pattern where
    hashWithSalt salt (Exact n)  = hashWithSalt salt n
    hashWithSalt salt (Prefix n) = hashWithSalt salt n

-- | This class represents types that can be automatically and safely
-- converted /from/ a 'Value' /to/ a destination type.  If conversion
-- fails because the types are not compatible, 'Nothing' is returned.
--
-- For an example of compatibility, a 'Value' of 'Bool' 'True' cannot
-- be 'convert'ed to an 'Int'.
class Configured a where
    convert :: Value -> Maybe a

    convertList :: Value -> Maybe [a]
    convertList (List xs) = mapM convert xs
    convertList _         = Nothing

instance Configured a => Configured [a] where
    convert = convertList

-- | An error occurred while processing a configuration file.
data ConfigError = ParseError FilePath String
                   deriving (Show, Typeable)

instance Exception ConfigError

-- | An error occurred while lookup up the given 'Name'.
data KeyError = KeyError Name
              deriving (Show, Typeable)

instance Exception KeyError

-- | Directions for automatically reloading 'Config' data.
data AutoConfig = AutoConfig {
      interval :: Int
    -- ^ Interval (in seconds) at which to check for updates to config
    -- files.  The smallest allowed interval is one second.
    , onError :: SomeException -> IO ()
    -- ^ Action invoked when an attempt to reload a 'Config' or notify
    -- a 'ChangeHandler' causes an exception to be thrown.
    --
    -- If this action rethrows its exception or throws a new
    -- exception, the modification checking thread will be killed.
    -- You may want your application to treat that as a fatal error,
    -- as its configuration may no longer be consistent.
    } deriving (Typeable)

instance Show AutoConfig where
    show c = "AutoConfig {interval = " ++ show (interval c) ++ "}"

-- | The name of a 'Config' value.
type Name = Text

-- | A packed 'FilePath'.
type Path = Text

-- | A name-value binding.
type Binding = (Name,Value)

-- | A directive in a configuration file.
data Directive = Import Path
               | Bind Name Value
               | Group Name [Directive]
                 deriving (Eq, Show, Typeable, Data)

-- | A value in a 'Config'.
data Value = Bool Bool
           -- ^ A Boolean. Represented in a configuration file as @on@
           -- or @off@, @true@ or @false@ (case sensitive).
           | String Text
           -- ^ A Unicode string.  Represented in a configuration file
           -- as text surrounded by double quotes.
           --
           -- Escape sequences:
           --
           -- * @\\n@ - newline
           --
           -- * @\\r@ - carriage return
           --
           -- * @\\t@ - horizontal tab
           --
           -- * @\\\\@ - backslash
           --
           -- * @\\\"@ - quotes
           --
           -- * @\\u@/xxxx/ - Unicode character, encoded as four
           --   hexadecimal digits
           --
           -- * @\\u@/xxxx/@\\u@/xxxx/ - Unicode character (as two
           --   UTF-16 surrogates)
           | Number Rational
           -- ^ Integer.
           | List [Value]
           -- ^ Heterogeneous list.  Represented in a configuration
           -- file as an opening square bracket \"@[@\", followed by a
           -- comma-separated series of values, ending with a closing
           -- square bracket \"@]@\".
             deriving (Eq, Show, Typeable, Data)

-- | An interpolation directive.
data Interpolate = Literal Text
                 | Interpolate Text
                   deriving (Eq, Show)
