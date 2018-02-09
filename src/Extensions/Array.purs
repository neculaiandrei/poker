module Extensions.Array where
  
import Data.Array (drop, elemIndex, length, nub, tail, zip, (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Prelude (class Eq, bind, pure, (+), (-), (==))
  
comb :: forall a. Eq a => Int -> Array a -> Array (Array a)
comb 0 n = [ [] ]
comb k n = do
  x  <- n
  let 
    xs = case elemIndex x n of
              Nothing -> []
              Just i -> drop (i+1) n

  ys  <- comb (k-1) xs
  pure (x:ys)

hasDuplicates :: forall a. Eq a => Array a -> Boolean
hasDuplicates xs = length xs == length (nub xs)

pairWithNext :: forall a. Array a -> Array (Tuple a a)
pairWithNext xs
  = case tail xs of
      Nothing -> []
      Just t  -> zip xs t