module Components.Cards.Queen
where

import Data.Poker (Suit(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude (show, ($), (<>))

queenImage :: Suit -> String
queenImage = case _ of
  Clubs -> "./assets/images/face-queen-club.png"
  Diamonds -> "./assets/images/face-queen-diamond.png"
  Hearts -> "./assets/images/face-queen-heart.png"
  Spades -> "./assets/images/face-queen-spade.png"

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
              [ HP.src $ queenImage s ]
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




