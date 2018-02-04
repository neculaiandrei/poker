module Components.Game where

import Prelude

import Data.Maybe (Maybe(..))
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
      [ HH.div 
          [ HP.class_ (H.ClassName "card") ]
          [ HH.div
              [ HP.class_ (H.ClassName "top-left-corner") ]
              [ HH.div 
                  [ HP.class_ (H.ClassName "number") ]
                  [ HH.text "4" ] 
              , HH.div 
                  [ HP.class_ (H.ClassName "small-symbol") ]
                  [ HH.text "♠" ]
              ]
          , HH.div
              [ HP.class_ (H.ClassName "main flex-stretched") ]
              [ HH.div
                [ HP.class_ (H.ClassName "row flex-stretched") ]
                [ HH.div 
                    [ HP.class_ (H.ClassName "big-symbol") ]
                    [ HH.text "♠" ]
                , HH.div 
                    [ HP.class_ (H.ClassName "big-symbol") ]
                    [ HH.text "♠" ]
                ]
              , HH.div
                [ HP.class_ (H.ClassName "row flex-stretched") ]
                [ HH.div 
                    [ HP.class_ (H.ClassName "big-symbol") ]
                    [ HH.text "♠" ]
                , HH.div 
                    [ HP.class_ (H.ClassName "big-symbol") ]
                    [ HH.text "♠" ]
                ]
              ]
          , HH.div
              [ HP.class_ (H.ClassName "right-bottom-corner") ]
              [ HH.div 
                  [ HP.class_ (H.ClassName "number") ]
                  [ HH.text "4" ] 
              , HH.div 
                  [ HP.class_ (H.ClassName "small-symbol") ]
                  [ HH.text "♠" ]
              ]
          ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Unit m
  eval (None next) = pure next


