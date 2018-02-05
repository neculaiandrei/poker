module Components.Game where

import Prelude

import Components.Cards.Ace as Ace
import Components.Cards.Back as Back
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
import Data.Maybe (Maybe(..))
import Data.Poker (Suit(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Boolean

data Query a = None a

myButton :: forall m. H.Component HH.HTML Query Unit Unit m
myButton =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = false

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div
      [ HP.class_ (H.ClassName "cards") ]
      (  renderWhole Clubs 
      <> renderWhole Hearts
      <> renderWhole Spades
      <> renderWhole Diamonds )

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

  eval :: Query ~> H.ComponentDSL State Query Unit m
  eval (None next) = pure next