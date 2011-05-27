module Data.Configurator.Types.Internal
    (
      Name
    , Path
    , Directive(..)
    , Value(..)
    ) where

import Data.Text (Text)

type Name = Text
type Path = Text

data Directive = Import Path
               | Bind Text Value
                 deriving (Eq, Show)

data Value = Bool Bool
           | String Text
           | Number Int
           | List [Value]
           | Group [Directive]
             deriving (Eq, Show)
