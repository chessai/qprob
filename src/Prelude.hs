module Prelude
  ( module P
  ) where

import Data.Tuple as P (fst,snd)
import Data.String as P (String)
import GHC.Show as P (Show(..))
import Data.Int as P (Int)
import Data.Bool as P (Bool(..),not)
import Data.Monoid as P (Monoid(..))
import Data.Semigroup as P (Semigroup(..))
import Data.Complex as P (Complex(..))
import Data.Functor as P (Functor(..))
import Data.Semiring as P (Semiring(..), Ring(..), (+), (*), (-))
import Control.Monad as P (Monad(..), (=<<))
import Control.Applicative as P (Applicative(..))
import Data.Eq as P (Eq(..))
import Data.Ord as P (Ord(..))
import GHC.Base as P (Double)
import Data.Function as P (($),(.),(&),id,flip)
import Data.Map as P (Map)
import GHC.Float as P (Floating(..))
import GHC.Real as P (fromIntegral, (/))
