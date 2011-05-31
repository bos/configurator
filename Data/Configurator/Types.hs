-- |
-- Module:      Data.Configurator.Types
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@mailrank.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with configuration files.

module Data.Configurator.Types
    (
      AutoConfig(..)
    , Config(cfgPaths)
    , Name
    , Value
    , Configured(..)
    ) where

import Data.Configurator.Types.Internal
