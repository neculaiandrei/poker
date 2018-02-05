module Data.Generator
where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Maybe (Maybe(..))
import Data.Poker (Card(Card), Suit(Spades, Hearts, Diamonds, Clubs), Hand)
import Data.Traversable (sequence)
import Prelude (bind, pure, (<<<), ($))

getCard :: Eff (random :: RANDOM) (Maybe Card)
getCard = do
  r <- randomInt 1 13
  s <- randomInt 1 4
  case s of
    1 -> pure <<< Just <<< Card r $ Clubs
    2 -> pure <<< Just <<< Card r $ Diamonds
    3 -> pure <<< Just <<< Card r $ Hearts
    4 -> pure <<< Just <<< Card r $ Spades
    _ -> pure Nothing

getHand :: Eff (random :: RANDOM) (Maybe Hand)
getHand = do
  h1 <- getCard
  h2 <- getCard
  h3 <- getCard
  h4 <- getCard
  h5 <- getCard
  
  pure <<< sequence $ [h1, h2, h3, h4, h5]
