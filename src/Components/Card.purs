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
import Data.Poker (Card(..), Rank(..))

render :: forall p i. Card -> HH.HTML p i
render (Card Two s) = Two.render s
render (Card Three s) = Three.render s
render (Card Four s) = Four.render s
render (Card Five s) = Five.render s
render (Card Six s) = Six.render s
render (Card Seven s) = Seven.render s
render (Card Eight s) = Eight.render s
render (Card Nine s) = Nine.render s
render (Card Ten s) = Ten.render s
render (Card Jack s) = Jack.render s
render (Card Queen s) = Queen.render s
render (Card King s) = King.render s
render (Card Ace s) = Ace.render s