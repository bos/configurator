{-# LANGUAGE DeriveDataTypeable #-}

module Data.Configurator.Types.Internal
    (
      Name
    , Value(..)
    , Binding
    , Path
    , Directive(..)
    , ConfigError(..)
    , Interp(..)
    ) where

import Control.Exception
import Data.Data (Data)
import Data.Text (Text)
import Data.Typeable (Typeable)

data ConfigError = ParseError FilePath String
                   deriving (Show, Typeable)

instance Exception ConfigError

type Name = Text
type Path = Text

type Binding = (Name,Value)

data Directive = Import Path
               | Bind Name Value
               | Group Name [Directive]
                 deriving (Eq, Show, Typeable, Data)

data Value = Bool Bool
           | String Text
           | Number Int
           | List [Value]
             deriving (Eq, Show, Typeable, Data)

data Interp = Literal Text
            | Interp Text
              deriving (Eq, Show)
