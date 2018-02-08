module Component.Card (
  render
) where
  
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
import Halogen.HTML as HH
import Poker.Types (Card(..))

render :: forall p i. Card -> HH.HTML p i
render (Card 2 s) = Two.render s
render (Card 3 s) = Three.render s
render (Card 4 s) = Four.render s
render (Card 5 s) = Five.render s
render (Card 6 s) = Six.render s
render (Card 7 s) = Seven.render s
render (Card 8 s) = Eight.render s
render (Card 9 s) = Nine.render s
render (Card 10 s) = Ten.render s
render (Card 11 s) = Jack.render s
render (Card 12 s) = Queen.render s
render (Card 13 s) = King.render s
render (Card 14 s) = Ace.render s
render _ = HH.div_ []

