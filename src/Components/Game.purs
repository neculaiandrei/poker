module Components.Game where

import Prelude

import Component.Card as CC
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Random (RANDOM)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Poker (Card(..), Hand, HandRank, generateHand, getHandRank, hand)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

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
  render h =
    HH.div_
      [ renderHand
      , renderHandRank <<< getHandRank $ h
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
            ( map CC.render (unwrap h) )

        renderHandRank :: Maybe HandRank -> H.ComponentHTML Query
        renderHandRank (Just r)  = HH.div_ [ HH.text $ "You've got " <> show r ]
        renderHandRank Nothing   = HH.div_ [ HH.text "Wtf is this hand?" ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (random :: RANDOM | eff))
  eval (Generate next) = do
    h <- H.liftEff generateHand
    case h of
      Just v -> do
        H.put v
        pure next
      Nothing -> do
        pure next
