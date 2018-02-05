module Poker.Hand.Generator (
  getHand
) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Array ((!!), (..))
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Unfoldable (replicateA)
import Poker.Types (Card(Card), Hand, Suit(..))
import Prelude (bind, pure, (<<<), ($))

buildDeck :: Array Card
buildDeck = do
  r <- 2 .. 14
  s <- [Clubs, Diamonds, Hearts, Spades]

  pure (Card r s)

getCard :: forall eff. Array Card -> Eff (random :: RANDOM | eff) (Maybe Card)
getCard d = do
  i <- randomInt 0 52

  case d !! i of
    Just c  -> pure <<< Just $ c
    Nothing -> pure Nothing

getHand :: forall eff. Eff (random :: RANDOM | eff) (Maybe Hand)
getHand = do
  let deck = buildDeck
  cards <- replicateA 5 (getCard deck)
  pure <<< sequence $ cards