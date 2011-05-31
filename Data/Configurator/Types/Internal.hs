{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module:      Data.Configurator.Types.Internal
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@mailrank.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with configuration files.

module Data.Configurator.Types.Internal
    (
      Config(..)
    , Configured(..)
    , AutoConfig(..)
    , Name
    , Value(..)
    , Binding
    , Path
    , Directive(..)
    , ConfigError(..)
    , Interp(..)
    ) where

import Control.Exception
import Data.Data (Data)
import Data.IORef (IORef)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Prelude hiding (lookup)
import qualified Data.HashMap.Lazy as H

-- | Configuration data.
data Config = Config {
      cfgPaths :: [Path]
    -- ^ The files from which the 'Config' was loaded.
    , cfgMap :: IORef (H.HashMap Name Value)
    }

-- | This class represents types that can be automatically and safely
-- converted from a 'Value'.  If conversion fails, 'Nothing' is
-- returned.
class Configured a where
    convert :: Value -> Maybe a

-- | An error occurred while processing a configuration file.
data ConfigError = ParseError FilePath String
                   deriving (Show, Typeable)

-- | Directions for automatically reloading 'Config' data.
data AutoConfig = AutoConfig {
      interval :: Int
    -- ^ Interval (in seconds) at which to check for updates to config
    -- files.  The smallest allowed interval is one second.
    , onError :: SomeException -> IO ()
    -- ^ Action invoked when an attempt to reload a 'Config' fails.
    -- If this action rethrows its exception or throws a new
    -- exception, the modification checking thread will be killed.
    } deriving (Typeable)

instance Show AutoConfig where
    show c = "AutoConfig {interval = " ++ show (interval c) ++ "}"

instance Exception ConfigError

-- | The name of a 'Config' value.
type Name = Text

-- | A packed 'FilePath'.
type Path = Text

-- | A name-value binding.
type Binding = (Name,Value)

-- | A directive in a config file.
data Directive = Import Path
               | Bind Name Value
               | Group Name [Directive]
                 deriving (Eq, Show, Typeable, Data)

-- | A value in a 'Config'.
data Value = Bool Bool
           | String Text
           | Number Int
           | List [Value]
             deriving (Eq, Show, Typeable, Data)

-- | An interpolation directive.
data Interp = Literal Text
            | Interp Text
              deriving (Eq, Show)
