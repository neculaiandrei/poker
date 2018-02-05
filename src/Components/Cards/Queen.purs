module Components.Cards.Queen
where

import Data.Poker (Suit)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude (show, (<>))
  
render :: forall p i. Suit -> HH.HTML p i
render s = 
  HH.div 
    [ HP.class_ (H.ClassName ("card card-" <> show s)) ]
    [ HH.div
        [ HP.class_ (H.ClassName "top-left-corner") ]
        [ HH.div 
            [ HP.class_ (H.ClassName "number") ]
            [ HH.text "Q" ] 
        , HH.div 
            [ HP.class_ (H.ClassName "small-symbol") ]
            [  ]
        ]
    , HH.div
        [ HP.class_ (H.ClassName "main flex-centered") ]
        [ HH.div
          [ HP.class_ (H.ClassName "row flex-centered") ]
          [ HH.img 
              [ HP.src "https://image.ibb.co/kq3Cm6/face_queen_heart.png" ]
          ]
        ]
    , HH.div
        [ HP.class_ (H.ClassName "right-bottom-corner") ]
        [ HH.div 
            [ HP.class_ (H.ClassName "number") ]
            [ HH.text "Q" ] 
        , HH.div 
            [ HP.class_ (H.ClassName "small-symbol") ]
            [  ]
        ]
    ]

