module Components.Cards.Seven
where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Data.Poker (Suit)
import Prelude (show, (<>))
  
render :: forall p i. Suit -> HH.HTML p i
render s = 
  HH.div 
    [ HP.class_ (H.ClassName ("card card-" <> show s)) ]
    [ HH.div
        [ HP.class_ (H.ClassName "top-left-corner") ]
        [ HH.div 
            [ HP.class_ (H.ClassName "number") ]
            [ HH.text "7" ] 
        , HH.div 
            [ HP.class_ (H.ClassName "small-symbol") ]
            []
        ]
    , HH.div
        [ HP.class_ (H.ClassName "main flex-stretched") ]
        [ HH.div
          [ HP.class_ (H.ClassName "row flex-stretched") ]
          [ HH.div 
              [ HP.class_ (H.ClassName "big-symbol") ]
              []
          , HH.div 
              [ HP.class_ (H.ClassName "big-symbol") ]
              []
          , HH.div 
              [ HP.class_ (H.ClassName "big-symbol") ]
              []
          ]
        , HH.div
          [ HP.class_ (H.ClassName "row flex-centered") ]
          [ HH.div 
              [ HP.class_ (H.ClassName "big-symbol") ]
              []
          ]
        , HH.div
          [ HP.class_ (H.ClassName "row flex-stretched") ]
          [ HH.div 
              [ HP.class_ (H.ClassName "big-symbol") ]
              []
          , HH.div 
              [ HP.class_ (H.ClassName "big-symbol") ]
              []
          , HH.div 
              [ HP.class_ (H.ClassName "big-symbol") ]
              []
          ]
        ]
    , HH.div
        [ HP.class_ (H.ClassName "right-bottom-corner") ]
        [ HH.div 
            [ HP.class_ (H.ClassName "number") ]
            [ HH.text "7" ] 
        , HH.div 
            [ HP.class_ (H.ClassName "small-symbol") ]
            []
        ]
    ]

