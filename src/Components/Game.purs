module Components.Game where

import Prelude

import Components.Cards.Ace as Ace
import Components.Cards.Eight as Eight
import Components.Cards.Five as Five
import Components.Cards.Four as Four
import Components.Cards.Jack as Jack
import Components.Cards.King as King
import Components.Cards.Nine as Nine
import Components.Cards.Queen as Queen
import Components.Cards.Seven as Seven
import Components.Cards.Six as Six
import Components.Cards.Ten as Ten
import Components.Cards.Three as Three
import Components.Cards.Two as Two
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Random (RANDOM)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Poker.Hand.Generator (getHand)
import Poker.Hand.RankCalculator (getHandRank)
import Poker.Types (Card(Card), Hand, HandRank)

type State = Hand

data Query a 
  = Generate a

component :: forall eff. H.Component HH.HTML Query Unit Void (Aff (random :: RANDOM | eff))
component =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action Generate)
    , finalizer: Nothing
    }
  where

  initialState :: Hand
  initialState = []

  render :: State -> H.ComponentHTML Query
  render hand =
    HH.div_
      [ renderHand
      , renderHandRank <<< getHandRank $ hand
      , HH.button
          [ HP.class_ (H.ClassName "new")
          , HE.onClick (HE.input_ Generate) ]
          [ HH.text "New hand" ]
      ]

      where 
        renderHand :: H.ComponentHTML Query
        renderHand = 
          HH.div
            [ HP.class_ (H.ClassName "cards") ]
            ( map renderCard hand )
        
        renderCard :: Card -> H.ComponentHTML Query
        renderCard (Card 2 s) = Two.render s
        renderCard (Card 3 s) = Three.render s
        renderCard (Card 4 s) = Four.render s
        renderCard (Card 5 s) = Five.render s
        renderCard (Card 6 s) = Six.render s
        renderCard (Card 7 s) = Seven.render s
        renderCard (Card 8 s) = Eight.render s
        renderCard (Card 9 s) = Nine.render s
        renderCard (Card 10 s) = Ten.render s
        renderCard (Card 11 s) = Jack.render s
        renderCard (Card 12 s) = Queen.render s
        renderCard (Card 13 s) = King.render s
        renderCard (Card 14 s) = Ace.render s
        renderCard _ = HH.div_ []

        renderHandRank :: Maybe HandRank -> H.ComponentHTML Query
        renderHandRank (Just r)  = HH.div_ [ HH.text $ "You've got " <> show r ]
        renderHandRank Nothing   = HH.div_ [ HH.text "Wtf is this hand?" ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (random :: RANDOM | eff))
  eval (Generate next) = do
    h <- H.liftEff getHand
    case h of
      Just v -> do
        H.put v
        pure next
      Nothing -> do
        pure next


