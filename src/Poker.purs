module Data.Poker where

import Data.Array (all, any, difference, drop, elemIndex, filter, find, head, intersect, sortBy, tail, zip, (:))
import Data.Foldable (length)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Prelude (class Eq, class Show, bind, compare, map, otherwise, pure, show, ($), (&&), (+), (-), (<<<), (<>), (==))

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
  = FiveOfAKind   --Rank
  | StraightFlush --Rank
  | FourOfAKind   --Rank Kicker
  | FullHouse     --Rank Rank
  | Flush         --Kicker Kicker Kicker Kicker Kicker
  | Straight      --Rank
  | ThreeOfAKind  --Rank Kicker Kicker
  | TwoPairs      --Rank Rank Kicker
  | OnePair       --Rank Kicker Kicker Kicker
  | HighCard      --Kicker Kicker Kicker Kicker Kicker

instance showHandValue :: Show HandValue where
  show FiveOfAKind = "FiveOfAKind"
  show StraightFlush = "StraightFlush"
  show FourOfAKind = "FourOfAKind"
  show FullHouse = "FullHouse"
  show Flush = "Flush"
  show Straight = "Straight"
  show ThreeOfAKind = "ThreeOfAKind"
  show TwoPairs = "TwoPairs"
  show OnePair = "OnePair"
  show HighCard = "HighCard"

getHandValue :: Hand -> HandValue
getHandValue h
  | hasStraightFlush h = StraightFlush
  | hasFourOfAKind   h = FourOfAKind
  | hasFlush         h = Flush
  | hasStraight      h = Straight
  | hasThreeOfAKind  h = ThreeOfAKind
  | hasTwoPairs      h = TwoPairs
  | hasOnePair       h = OnePair
  | otherwise          = HighCard
  

handExample :: Hand
handExample = [
  Card 9 Diamonds,
  Card 8 Hearts,
  Card 10 Diamonds,
  Card 6 Spades,
  Card 7 Diamonds
]

hasOnePair :: Hand -> Boolean
hasOnePair = (_ == 1) <<< length <<< getPairs 

hasTwoPairs :: Hand -> Boolean
hasTwoPairs = (_ == 2) <<< length <<< getPairs

hasThreeOfAKind :: Hand -> Boolean
hasThreeOfAKind h = any isThreeOfAKind (comb h 3)
  where
    isThreeOfAKind [Card r1 _, Card r2 _, Card r3 _] = r1 == r2 && r2 == r3
    isThreeOfAKind _ = false

hasStraight :: Hand -> Boolean
hasStraight = isStraight <<< sortBy (\(Card r1 _) (Card r2 _) -> compare r1 r2)
  where
    isStraight [Card r1 _, Card r2 _, Card r3 _, Card r4 _, Card r5 _]
      =  (r1 + 1) == r2
      && (r2 + 1) == r3
      && (r3 + 1) == r4
      && (r4 + 1) == r5
    isStraight _ = false

hasFlush :: Hand -> Boolean
hasFlush h 
  = case tail h of
      Nothing -> true
      Just t -> all areSameSuit (zip h t)

      where
        areSameSuit (Tuple (Card _ s1) (Card _ s2)) = s1 == s2

hasFourOfAKind :: Hand -> Boolean
hasFourOfAKind h = any isFourOfAKind (comb h 4)
  where
    isFourOfAKind [Card r1 _, Card r2 _, Card r3 _, Card r4 _] 
      =  r1 == r2 
      && r2 == r3 
      && r3 == r4
    isFourOfAKind _ = false

-- hasFourOfAKind' :: Hand -> Maybe HandValue
-- hasFourOfAKind' h 
--   = case find isFourOfAKind (comb h 4) of
--       Nothing -> Nothing
--       (Just v) -> Just <<< FourOfAKind (getRank v) $ (getKicker h v)

--     where
--       isFourOfAKind [Card r1 _, Card r2 _, Card r3 _, Card r4 _] 
--         =  r1 == r2 
--         && r2 == r3 
--         && r3 == r4
--       isFourOfAKind _ = false

--       getRank [Card r1 _, _ , _, _] = r1
--       getRank _ = 0
--       getKicker h v 
--         = case head (getKickers h v) of
--             Nothing -> 0
--             Just k  -> k

hasStraightFlush :: Hand -> Boolean
hasStraightFlush h = hasFlush h && hasStraight h


getKickers :: Hand -> Array Card -> Array Kicker
getKickers h = (map (\(Card r _) -> r)) <<< sortCards <<< difference h


sortCards :: Array Card -> Array Card
sortCards = sortBy (\(Card r1 _) (Card r2 _) -> compare r1 r2)

getPairs :: Hand -> Array (Array Card)
getPairs h = filter isPair (comb h 2)
  where
    isPair [Card r1 _, Card r2 _] = r1 == r2
    isPair _ = false


comb :: forall a. Eq a => Array a -> Int -> Array (Array a)
comb n 0 = [[]]
comb n k = do
  x  <- n
  let xs = case elemIndex x n of
              Nothing -> []
              Just i -> drop (i+1) n

  ys  <- comb xs (k - 1)
  pure (x : ys)
