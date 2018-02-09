module Poker.Types (
  Rank(..),
  Kicker,
  Suit(..),
  Card(..),
  Hand,
  HandRank(..)
) where

import Data.Enum (class BoundedEnum, class Enum, Cardinality(Cardinality), defaultPred, defaultSucc, fromEnum)
import Data.Maybe (Maybe(..))
import Prelude (class Bounded, class Eq, class Ord, class Show, show, ($), (<<<), (<>))

data Rank 
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

derive instance eqRank :: Eq Rank
derive instance ordRank :: Ord Rank
instance enumRank :: Enum Rank where
  succ = defaultSucc toEnumRank fromEnumRank
  pred = defaultPred toEnumRank fromEnumRank

instance boundedRank :: Bounded Rank where
  bottom = Two
  top = Ace

instance boundedEnumRank :: BoundedEnum Rank where
  cardinality = Cardinality 13
  toEnum = toEnumRank
  fromEnum = fromEnumRank

toEnumRank :: Int -> Maybe Rank
toEnumRank =
  case _ of
    1   -> Just Ace
    2   -> Just Two
    3   -> Just Three
    4   -> Just Four
    5   -> Just Five
    6   -> Just Six
    7   -> Just Seven
    8   -> Just Eight
    9   -> Just Nine
    10  -> Just Ten
    11  -> Just Jack
    12  -> Just Queen
    13  -> Just King
    _   -> Nothing

fromEnumRank :: Rank -> Int
fromEnumRank =
  case _ of
    Ace   -> 1
    Two   -> 2
    Three -> 3
    Four  -> 4
    Five  -> 5
    Six   -> 6
    Seven -> 7
    Eight -> 8
    Nine  -> 9
    Ten   -> 10
    Jack  -> 11
    Queen -> 12
    King  -> 13

instance showRank :: Show Rank where
  show =
    case _ of
      Jack  -> "Jack"
      Queen -> "Queen"
      King  -> "King"
      Ace   -> "Ace"
      r     -> show <<< fromEnum $ r

type Kicker = Rank

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