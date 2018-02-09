module Data.Poker.HandGenerator (
  generateHand
) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.State (StateT, evalStateT, get, lift, put)
import Data.Array (filter, length, (!!))
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Unfoldable (replicateA)
import Data.Poker.Card (Card(..), Rank(..), Suit(..))
import Prelude (bind, discard, pure, ($), (-), (/=), (<<<))

type Deck = Array Card

makeDeck :: Deck
makeDeck = do
  r <- [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]
  s <- [Clubs, Diamonds, Hearts, Spades]

  pure (Card r s)

generateCard :: forall eff. StateT Deck (Eff (random :: RANDOM | eff)) (Maybe Card)
generateCard = do
  st <- get
  i <- lift (randomInt 0 (length st - 1))

  case st !! i of
    Just selectedCard -> do
      let newSt = filter (\c-> c /= selectedCard) st
      put newSt
      pure <<< Just $ selectedCard
    Nothing ->
      pure Nothing

generateHand :: forall eff. Eff ( random :: RANDOM | eff) (Maybe (Array Card))
generateHand = evalStateT (do
  cards <- replicateA 5 generateCard
  pure <<< sequence $ cards
  ) makeDeck