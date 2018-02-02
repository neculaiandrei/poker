module Data.Poker (
  Rank,
  Kicker,
  Suit,
  Card,
  Hand,
  HandValue,
  getHandValue
) where

import Control.Alt ((<|>))
import Control.MonadZero (guard)
import Data.Array (all, concat, difference, filter, find, head, length, reverse, sortBy)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Extensions.Array (comb, pairWithNext)
import Prelude (class Eq, class Ord, class Show, bind, compare, discard, map, pure, show, ($), (-), (<<<), (<>), (==))

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

data HandValue
  = HighCard      Kicker Kicker Kicker Kicker Kicker
  | OnePair       Rank Kicker Kicker Kicker
  | TwoPairs      Rank Rank Kicker
  | ThreeOfAKind  Rank Kicker Kicker
  | Straight      Rank
  | Flush         Rank
  | FullHouse     Rank Rank
  | FourOfAKind   Rank Kicker
  | StraightFlush Rank

instance showHandValue :: Show HandValue where
  show (StraightFlush r)    = show r <> " high straight flush"
  show (FourOfAKind r k)    = "Four of a kind, " <> show r <> "s"
  show (FullHouse r1 r2)    = "Full house, " <> show r1 <> "s over " <> show r2 <> "s"
  show (Flush r)            = show r <> " high flush"
  show (Straight r)         = show r <> " high straight"
  show (ThreeOfAKind r _ _) = "Three " <> show r <> "s"
  show (TwoPairs r1 r2 _)   = "Two pairs, " <> show r1 <> "s and " <> show r2 <> "s" 
  show (OnePair r _ _ _)    = "Pair of " <> show r <> "s"
  show (HighCard k _ _ _ _) = "High card, " <> show k
derive instance eqHandValue :: Eq HandValue
derive instance ordHandValue :: Ord HandValue

getHandValue :: Hand -> Maybe HandValue
getHandValue = getHandValue' <<< sortCards

getHandValue' :: Hand -> Maybe HandValue
getHandValue' h 
  =   hasStraightFlush h
  <|> hasFourOfAKind h
  <|> hasFullHouse h
  <|> hasFlush h
  <|> hasStraight h
  <|> hasThreeOfAKind h
  <|> hasTwoPairs h
  <|> hasOnePair h
  <|> (Just <<< hasHighCard $ h)

handExample :: Hand
handExample = [
  Card 9 Diamonds,
  Card 9 Hearts,
  Card 11 Diamonds,
  Card 10 Spades,
  Card 9 Diamonds
]

hasStraightFlush :: Hand -> Maybe HandValue
hasStraightFlush h = do 
  x <- hasFlush h
  y <- hasStraight h
  let 
    getRank :: HandValue -> Rank
    getRank (Straight r) = r 
    getRank _ = 0

  pure <<< StraightFlush <<< getRank $ y

hasFourOfAKind :: Hand -> Maybe HandValue
hasFourOfAKind h = do
  fours <- find areSameRank (comb 4 h)
  wrap (getRank fours) (getKickers h fours) 
    where

      wrap :: Rank -> (Array Kicker) -> Maybe HandValue
      wrap r [k] = Just (FourOfAKind r k)
      wrap _ _ = Nothing

hasFullHouse :: Hand -> Maybe HandValue
hasFullHouse h = do
  threes <- hasThreeOfAKind h
  let
    threesAreFulls (ThreeOfAKind r k1 k2) = do
      guard $ k1 == k2
      pure <<< FullHouse r $ k1
    threesAreFulls _ = Nothing
  
  threesAreFulls threes

hasFlush :: Hand -> Maybe HandValue
hasFlush h = do
  guard $ areSameSuit h
  pure <<< Flush <<< getRank $ h
  
hasStraight :: Hand -> Maybe HandValue
hasStraight h = do 
  guard $ areStraight h
  pure <<< Straight <<< getRank $ h

hasThreeOfAKind :: Hand -> Maybe HandValue
hasThreeOfAKind h = do 
  threes <- find areSameRank (comb 3 h)
  wrap (getRank threes) (getKickers h threes) 
    where

      wrap :: Rank -> (Array Kicker) -> Maybe HandValue
      wrap r [k1, k2] = Just (ThreeOfAKind r k1 k2)
      wrap _ _ = Nothing

hasTwoPairs :: Hand -> Maybe HandValue
hasTwoPairs h = do
  let pairs = filter areSameRank (comb 2 h)
  guard $ length pairs == 2
  wrap (getRankOfPairs pairs) (getKickers h (concat pairs))
  where

    getRankOfPairs :: Array (Array Card) -> Array Rank
    getRankOfPairs [p1, p2] = [getRank p1, getRank p2]
    getRankOfPairs _ = []

    wrap :: Array Rank -> Array Kicker -> Maybe HandValue
    wrap [r1, r2] [k1] = Just (TwoPairs r1 r2 k1)
    wrap _ _ = Nothing

hasOnePair :: Hand -> Maybe HandValue
hasOnePair h = do 
  p <- find areSameRank (comb 2 h)
  wrap (getRank p) (getKickers h p) 
    where

    wrap :: Rank -> Array Kicker -> Maybe HandValue
    wrap r [k1, k2, k3] = Just (OnePair r k1 k2 k3)
    wrap _ _ = Nothing

hasHighCard :: Hand -> HandValue
hasHighCard h = wrap <<< getKickers h $ []
  where
    
    wrap :: Array Kicker -> HandValue
    wrap [k1, k2, k3, k4, k5] = HighCard k1 k2 k3 k4 k5
    wrap _ = HighCard 0 0 0 0 0

areSameRank :: Array Card -> Boolean
areSameRank = all restriction <<< pairWithNext
  where
    restriction (Tuple (Card r1 _) (Card r2 _)) = r1 == r2

areSameSuit :: Array Card -> Boolean
areSameSuit = all restriction <<< pairWithNext
  where
    restriction (Tuple (Card _ s1) (Card _ s2)) = s1 == s2

areStraight :: Array Card -> Boolean
areStraight = all restriction <<< pairWithNext
  where
    restriction (Tuple (Card r1 _) (Card r2 _)) = r1 - 1 == r2

getKickers :: Hand -> Array Card -> Array Kicker
getKickers h = (map (\(Card r _) -> r)) <<< difference h

getRank :: Array Card -> Rank
getRank h = case head h of
  Nothing -> 0
  Just (Card r _) -> r

sortCards :: Array Card -> Array Card
sortCards = reverse <<< sortBy (\(Card r1 _) (Card r2 _) -> compare r1 r2)
