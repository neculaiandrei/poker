module Data.Poker.Hand (
  Hand(..),
  HandRank(..),
  Kicker(..),
  hand
) where

import Control.MonadZero (guard)
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Poker.Card (Card, Rank)
import Extensions.Array (hasDuplicates)
import Prelude (class Eq, class Ord, class Show, discard, show, ($), (<<<), (<>), (==))

newtype Hand = Hand (Array Card)

derive instance newtypeHand :: Newtype Hand _

type Kicker = Rank

data HandRank
  = HighCard      Kicker Kicker Kicker Kicker Kicker
  | OnePair       Rank Kicker Kicker Kicker
  | TwoPairs      Rank Rank Kicker
  | ThreeOfAKind  Rank Kicker Kicker
  | Straight      Rank
  | Flush         Rank
  | FullHouse     Rank Rank
  | FourOfAKind   Rank Kicker
  | StraightFlush Rank

derive instance eqHandRank :: Eq HandRank
derive instance ordHandRank :: Ord HandRank

instance showHandRank :: Show HandRank where
  show (StraightFlush r)    = show r <> " high straight flush"
  show (FourOfAKind r k)    = "Four of a kind, " <> show r <> "s"
  show (FullHouse r1 r2)    = "Full house, " <> show r1 <> "s over " <> show r2 <> "s"
  show (Flush r)            = show r <> " high flush"
  show (Straight r)         = show r <> " high straight"
  show (ThreeOfAKind r _ _) = "Three " <> show r <> "s"
  show (TwoPairs r1 r2 _)   = "Two pairs, " <> show r1 <> "s and " <> show r2 <> "s" 
  show (OnePair r _ _ _)    = "Pair of " <> show r <> "s"
  show (HighCard k _ _ _ _) = "High card, " <> show k


hand :: Array Card -> Maybe Hand
hand xs = do
  guard $ length xs == 5
  guard $ hasDuplicates xs
  Just <<< Hand $ xs
