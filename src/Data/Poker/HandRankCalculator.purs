module Data.Poker.HandRankCalculator (
  getHandRank
) where

import Control.Alt ((<|>))
import Control.MonadZero (guard)
import Data.Array (all, concat, difference, filter, find, head, length, reverse, sortBy)
import Data.Enum (pred)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Extensions.Array (comb, pairWithNext)
import Data.Poker.Card (Card(..), Rank(..), Suit(..))
import Data.Poker.Hand (Hand, HandRank(..), Kicker)
import Prelude (bind, compare, discard, map, pure, ($), (<<<), (==))

getHandRank :: Hand -> Maybe HandRank
getHandRank = getHandRank' <<< sortCards

getHandRank' :: Hand -> Maybe HandRank
getHandRank' h 
  =   hasStraightFlush h
  <|> hasFourOfAKind h
  <|> hasFullHouse h
  <|> hasFlush h
  <|> hasStraight h
  <|> hasThreeOfAKind h
  <|> hasTwoPairs h
  <|> hasOnePair h
  <|> hasHighCard h

handExample :: Hand
handExample = [
  Card Nine Diamonds,
  Card Nine Hearts,
  Card Jack Diamonds,
  Card Ten Spades, 
  Card Nine Diamonds
]

hasStraightFlush :: Hand -> Maybe HandRank
hasStraightFlush h = do 
  x <- hasFlush h
  y <- hasStraight h
  let 
    getHighestCard :: HandRank -> Rank
    getHighestCard (Straight r) = r 
    getHighestCard _ = Two

  pure <<< StraightFlush <<< getHighestCard $ y

hasFourOfAKind :: Hand -> Maybe HandRank
hasFourOfAKind h = do
  fours <- find areSameRank (comb 4 h)
  wrap (getHighestCard fours) (getKickers h fours) 
    where

      wrap :: Rank -> (Array Kicker) -> Maybe HandRank
      wrap r [k] = Just (FourOfAKind r k)
      wrap _ _ = Nothing

hasFullHouse :: Hand -> Maybe HandRank
hasFullHouse h = do
  threes <- hasThreeOfAKind h
  let
    threesAreFulls (ThreeOfAKind r k1 k2) = do
      guard $ k1 == k2
      pure <<< FullHouse r $ k1
    threesAreFulls _ = Nothing
  
  threesAreFulls threes

hasFlush :: Hand -> Maybe HandRank
hasFlush h = do
  guard $ areSameSuit h
  pure <<< Flush <<< getHighestCard $ h
  
hasStraight :: Hand -> Maybe HandRank
hasStraight h = do 
  guard $ areStraight h
  pure <<< Straight <<< getHighestCard $ h

hasThreeOfAKind :: Hand -> Maybe HandRank
hasThreeOfAKind h = do 
  threes <- find areSameRank (comb 3 h)
  wrap (getHighestCard threes) (getKickers h threes) 
    where

      wrap :: Rank -> (Array Kicker) -> Maybe HandRank
      wrap r [k1, k2] = Just (ThreeOfAKind r k1 k2)
      wrap _ _ = Nothing

hasTwoPairs :: Hand -> Maybe HandRank
hasTwoPairs h = do
  let pairs = filter areSameRank (comb 2 h)
  guard $ length pairs == 2
  wrap (getRankOfPairs pairs) (getKickers h (concat pairs))
  where

    getRankOfPairs :: Array (Array Card) -> Array Rank
    getRankOfPairs [p1, p2] = [getHighestCard p1, getHighestCard p2]
    getRankOfPairs _ = []

    wrap :: Array Rank -> Array Kicker -> Maybe HandRank
    wrap [r1, r2] [k1] = Just (TwoPairs r1 r2 k1)
    wrap _ _ = Nothing

hasOnePair :: Hand -> Maybe HandRank
hasOnePair h = do 
  p <- find areSameRank (comb 2 h)
  wrap (getHighestCard p) (getKickers h p) 
    where

    wrap :: Rank -> Array Kicker -> Maybe HandRank
    wrap r [k1, k2, k3] = Just (OnePair r k1 k2 k3)
    wrap _ _ = Nothing

hasHighCard :: Hand -> Maybe HandRank
hasHighCard h = wrap <<< getKickers h $ []
  where
    
    wrap :: Array Kicker -> Maybe HandRank
    wrap [k1, k2, k3, k4, k5] = Just (HighCard k1 k2 k3 k4 k5)
    wrap _ = Nothing

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
    restriction (Tuple (Card r1 _) (Card r2 _)) = pred r1 == Just r2

getKickers :: Hand -> Array Card -> Array Kicker
getKickers h = (map (\(Card r _) -> r)) <<< difference h

getHighestCard :: Array Card -> Rank
getHighestCard h = case head h of
  Nothing -> Two
  Just (Card r _) -> r

sortCards :: Array Card -> Array Card
sortCards = reverse <<< sortBy (\(Card r1 _) (Card r2 _) -> compare r1 r2)



