module Data.Poker.Card (
  Card(..),
  Rank(..),
  Suit(..)
) where

import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), defaultPred, defaultSucc, fromEnum)
import Data.Maybe (Maybe(..))
import Prelude (class Bounded, class Eq, class Ord, class Show, show, ($), (<<<), (<>))

data Card = Card Rank Suit

derive instance eqCard :: Eq Card

instance showCard :: Show Card where
  show (Card r s) = show r <> " of " <> show s

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

data Suit
  = Diamonds
  | Clubs
  | Hearts
  | Spades

derive instance eqSuit :: Eq Suit

instance showSuit :: Show Suit where
  show Diamonds = "Diamonds"
  show Clubs    = "Clubs"
  show Hearts   = "Hearts"
  show Spades   = "Spades"


