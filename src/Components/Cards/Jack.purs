module Components.Cards.Jack
where

import Data.Poker (Suit(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude (show, ($), (<>))

jackImage :: Suit -> String
jackImage = case _ of
  Clubs -> "./assets/images/face-jack-club.png"
  Diamonds -> "./assets/images/face-jack-diamond.png"
  Hearts -> "./assets/images/face-jack-heart.png"
  Spades -> "./assets/images/face-jack-spade.png"
      
render :: forall p i. Suit -> HH.HTML p i
render s = 
  HH.div 
    [ HP.class_ (H.ClassName ("card card-" <> show s)) ]
    [ HH.div
        [ HP.class_ (H.ClassName "top-left-corner") ]
        [ HH.div 
            [ HP.class_ (H.ClassName "number") ]
            [ HH.text "J" ] 
        , HH.div 
            [ HP.class_ (H.ClassName "small-symbol") ]
            []
        ]
    , HH.div
        [ HP.class_ (H.ClassName "main flex-centered") ]
        [ HH.div
          [ HP.class_ (H.ClassName "row flex-centered") ]
          [ HH.img 
              [ HP.src $ jackImage s ]
          ]
        ]
    , HH.div
        [ HP.class_ (H.ClassName "right-bottom-corner") ]
        [ HH.div 
            [ HP.class_ (H.ClassName "number") ]
            [ HH.text "J" ] 
        , HH.div 
            [ HP.class_ (H.ClassName "small-symbol") ]
            []
        ]
    ]



