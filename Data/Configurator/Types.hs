-- |
-- Module:      Data.Configurator.Types
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with configuration files.

module Data.Configurator.Types
    (
      AutoConfig(..)
    , Config
    , Name
    , Value(..)
    , Configured(..)
    , Worth(..)
    -- * Exceptions
    , ConfigError(..)
    , KeyError(..)
    -- * Notification of configuration changes
    , Pattern
    , ChangeHandler
    -- * Types for generating config files
    , FileEntry(..)
    , ConfigFile
    ) where

import Data.Configurator.Types.Internal
