module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.MonadZero (guard)
import Data.Array (length, nub)
import Data.Maybe (Maybe(..))
import Poker.Hand.Generator (getHand)
import Test.QuickCheck (quickCheck)

-- main :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION, random :: RANDOM | e) Unit
-- main = do
--   quickCheck handHasNoDuplicates


-- handHasNoDuplicates :: forall e. Eff (random :: RANDOM | e) (Boolean)
-- handHasNoDuplicates = do
--   h <- getHand
--   case h of
--     Nothing   -> pure false
--     (Just h') -> pure (length h' == length (nub h'))