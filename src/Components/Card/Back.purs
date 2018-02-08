module Components.Cards.Back
where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Poker.Types (Suit)

render :: forall p i. Suit -> HH.HTML p i
render s = 
  HH.div 
    [ HP.class_ (H.ClassName "card") ]
    [ HH.div_
        [ HH.img 
            [ HP.src "https://image.ibb.co/djUV66/back_2.png" ]
        ]
    ]

