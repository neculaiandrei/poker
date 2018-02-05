module Poker.Types (
  Rank,
  Kicker,
  Suit(..),
  Card(..),
  Hand,
  HandRank(..)
) where

import Prelude (class Eq, class Ord, class Show, show, (<>))

type Rank   = Int
type Kicker = Int

data Suit
  = Diamonds
  | Clubs
  | Hearts
  | Spades

instance showSuit :: Show Suit where
  show Diamonds = "Diamonds"
  show Clubs    = "Clubs"
  show Hearts   = "Hearts"
  show Spades   = "Spades"
derive instance eqSuit :: Eq Suit

data Card = Card Rank Suit

instance showCard :: Show Card where
  show (Card r s) = show r <> " of " <> show s
derive instance eqCard :: Eq Card

type Hand = Array Card

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
derive instance eqHandRank :: Eq HandRank
derive instance ordHandRank :: Ord HandRank