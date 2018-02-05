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
import Poker.Types (Hand, Suit(Diamonds, Spades, Hearts, Clubs))

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
  render state =
    HH.div
      [ HP.class_ (H.ClassName "cards") ]
      (  renderWhole Clubs 
      <> renderWhole Hearts
      <> renderWhole Spades
      <> renderWhole Diamonds
      <> [ HH.div_ [ HH.text <<< show $ state ]
         , HH.button
            [ HE.onClick (HE.input_ Generate) ]
            [ HH.text "Generate new hand" ] ] )

      where 
        renderWhole s = 
          [ Ace.render s
          , Two.render s
          , Three.render s
          , Four.render s
          , Five.render s
          , Six.render s
          , Seven.render s
          , Eight.render s
          , Nine.render s
          , Ten.render s
          , Jack.render s
          , Queen.render s
          , King.render s ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (random :: RANDOM | eff))
  eval (Generate next) = do
    h <- H.liftEff getHand
    case h of
      Just v -> do
        H.put v
        pure next
      Nothing -> do
        pure next

