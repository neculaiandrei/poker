module Data.Poker where

import Control.Alt ((<|>))
import Data.Array (all, concat, difference, drop, elemIndex, filter, find, head, reverse, sortBy, tail, zip, (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Prelude (class Eq, class Ord, class Show, bind, compare, map, pure, show, ($), (+), (-), (<<<), (<>), (==))

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
  
derive instance eqHandValue :: Eq HandValue
derive instance ordHandValue :: Ord HandValue

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

getHandValue :: Hand -> Maybe HandValue
getHandValue h 
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
  Card 1 Hearts,
  Card 11 Diamonds,
  Card 10 Spades,
  Card 7 Diamonds
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
hasFourOfAKind h = case find (all areSameRank <<< pairWithNext) (comb 4 h) of
  Nothing -> Nothing
  Just p  -> construct (getRank p) (getKickers h p) 
    where

      construct :: Rank -> (Array Kicker) -> Maybe HandValue
      construct r [k] = Just (FourOfAKind r k)
      construct _ _ = Nothing

hasFullHouse :: Hand -> Maybe HandValue
hasFullHouse h = do
  t <- hasThreeOfAKind h
  let
    threesAreFulls (ThreeOfAKind r k1 k2) = case k1 == k2 of
      false -> Nothing
      true  -> Just <<< FullHouse r $ k1
    threesAreFulls _ = Nothing
  
  threesAreFulls t

hasFlush :: Hand -> Maybe HandValue
hasFlush h = case all areSameSuit <<< pairWithNext $ h of
  false -> Nothing
  true -> Just <<< Flush <<< getRank <<< reverse <<< sortCards $ h
  
hasStraight :: Hand -> Maybe HandValue
hasStraight h = case all areSucc <<< pairWithNext <<< sortCards $ h of
  false -> Nothing
  true  -> Just <<< Straight <<< getRank <<< reverse <<< sortCards $ h

hasThreeOfAKind :: Hand -> Maybe HandValue
hasThreeOfAKind h = case find (all areSameRank <<< pairWithNext) (comb 3 h) of
  Nothing -> Nothing
  Just p  -> construct (getRank p) (getKickers h p) 
    where

      construct :: Rank -> (Array Kicker) -> Maybe HandValue
      construct r [k1, k2] = Just (ThreeOfAKind r k1 k2)
      construct _ _ = Nothing

hasTwoPairs :: Hand -> Maybe HandValue
hasTwoPairs h = construct (getRank p) (getKickers h (concat p))
  where
    p :: Array (Array Card)
    p = getPairs

    getPairs :: Array (Array Card)
    getPairs = filter (all areSameRank <<< pairWithNext) (comb 2 h)

    getRank :: Array (Array Card) -> Array Rank
    getRank [[(Card r1 _), _], [(Card r2 _), _]] = [r1, r2]
    getRank _ = []

    construct :: Array Rank -> Array Kicker -> Maybe HandValue
    construct [r1, r2] [k1] = Just (TwoPairs r1 r2 k1)
    construct _ _ = Nothing

hasOnePair :: Hand -> Maybe HandValue
hasOnePair h = case find (all areSameRank <<< pairWithNext) (comb 2 h) of
  Nothing -> Nothing
  Just p  -> construct (getRank p) (getKickers h p) 
    where

    construct :: Rank -> Array Kicker -> Maybe HandValue
    construct r [k1, k2, k3] = Just (OnePair r k1 k2 k3)
    construct _ _ = Nothing

hasHighCard :: Hand -> HandValue
hasHighCard h = construct <<< getKickers h $ []
  where
    
    construct :: Array Kicker -> HandValue
    construct [k1, k2, k3, k4, k5] = HighCard k1 k2 k3 k4 k5
    construct _ = HighCard 0 0 0 0 0

areSameRank :: Tuple Card Card -> Boolean
areSameRank (Tuple (Card r1 _) (Card r2 _)) = r1 == r2

areSameSuit :: Tuple Card Card -> Boolean
areSameSuit (Tuple (Card _ s1) (Card _ s2)) = s1 == s2

areSucc :: Tuple Card Card -> Boolean
areSucc (Tuple (Card r1 _) (Card r2 _)) = r1 + 1 == r2

getKickers :: Hand -> Array Card -> Array Kicker
getKickers h = (map (\(Card r _) -> r)) <<< reverse <<< sortCards <<< difference h

getRank :: Array Card -> Rank
getRank h = case head h of
  Nothing -> 0
  Just (Card r _) -> r

pairWithNext :: forall a. Array a -> Array (Tuple a a)
pairWithNext xs
  = case tail xs of
      Nothing -> []
      Just t  -> zip xs t

sortCards :: Array Card -> Array Card
sortCards = sortBy (\(Card r1 _) (Card r2 _) -> compare r1 r2)

comb :: forall a. Eq a => Int -> Array a -> Array (Array a)
comb 0 n = [ [] ]
comb k n = do
  x  <- n
  let xs = case elemIndex x n of
              Nothing -> []
              Just i -> drop (i+1) n

  ys  <- comb (k - 1) xs
  pure (x : ys)

