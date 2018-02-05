module Poker.Hand.Generator (
  getHand
) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Unfoldable (replicateA)
import Poker.Types (Card(Card), Suit(Spades, Hearts, Diamonds, Clubs), Hand)
import Prelude (bind, pure, (<<<), ($))

getCard :: forall eff. Eff (random :: RANDOM | eff) (Maybe Card)
getCard = do
  r <- randomInt 2 14
  s <- randomInt 1 4
  case s of
    1 -> pure <<< Just <<< Card r $ Clubs
    2 -> pure <<< Just <<< Card r $ Diamonds
    3 -> pure <<< Just <<< Card r $ Hearts
    4 -> pure <<< Just <<< Card r $ Spades
    _ -> pure Nothing

getHand :: forall eff. Eff (random :: RANDOM | eff) (Maybe Hand)
getHand = do
  cards <- replicateA 5 getCard
  pure <<< sequence $ cards




