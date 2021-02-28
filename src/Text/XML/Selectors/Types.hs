-- | Description: Defunctionalized representation of selectors
module Text.XML.Selectors.Types
where

import Text.XML
import Text.XML.Cursor
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Applicative

-- | Node-level selectors and combinators
data Selector
  = None
  | Any -- ^ @*@
  | Append Selector Selector -- ^ @ab@ (both a and b must match)
  | Elem Name -- ^ @div@
  | Attrib AttribSelector -- ^ @a[...]@
  | Descendant -- ^ @ @ (whitespace)
  | Child -- ^ @>@
  | Sibling -- ^ @~@
  | NextSibling -- ^ @+@
  | FirstChild -- ^ @:first-child@
  | LastChild -- ^ @:last-child@
  | NthChild Int -- ^ @:nth-child(n); :nth-last-child(-n)@
  | Choice [Selector] -- ^ @a,b,...@
  | Having Selector -- ^ @:has(b)@
  | Not Selector -- ^ @:not(b)@
  deriving (Show)

selectorAppend :: Selector -> Selector -> Selector
selectorAppend Any x = x
selectorAppend x Any = x
selectorAppend a b = Append a b

-- | The 'Semigroup' of selectors combines selectors with 'Append'. @a <> b@
-- selects all nodes that match @a@ and also match @b@. Note however that the
-- '<>' operator culls redundant combinations with the 'Any' selector, e.g.
-- @Any <> Child <> Any@ is just @Child@, not @Append Any (Append Child Any)@.
instance Semigroup Selector where
  (<>) = selectorAppend

-- | The 'Monoid' instance, just like 'Semigroup', combines selectors with
-- 'Append'; the neutral value is, of course, 'Any'.
instance Monoid Selector where
  mappend = (<>)
  mempty = Any

-- | An alternative semigroup of selectors, representing choice.
-- @a \<||\> b@ selects all nodes that match @a@ and also all nodes that match
-- @b@. In other words: @a \<||\> b@ == @Choice a b@. Note however that the
-- '<||>' operator culls redundant applications of 'Choice', e.g.,
-- @a \<||\> b \<||\> c@ becomes @Choice [a, b, c]@ rather than
-- @Choice [a, Choice [b, c]]@. This alternative semigroup could be extended
-- into a monoid, with the empty choice (@Choice []@) as the neutral value,
-- but we were far too lazy to add that.
(<||>) :: Selector -> Selector -> Selector
Choice xs <||> Choice ys = Choice (xs ++ ys)
Choice xs <||> y = Choice (xs ++ [y])
x <||> Choice ys = Choice (x : ys)
x <||> y = Choice [x, y]
infixl 3 <||>

-- | Attribute-level selectors
data AttribSelector
  = AttribExists Name -- ^ @[attr]@
  | AttribIs Name Text -- ^ @[attr=blah]@
  | AttribIsNot Name Text -- ^ @[attr!=blah]@
  | AttribStartsWith Name Text -- ^ @[attr^=blah]@
  | AttribEndsWith Name Text -- ^ @[attr$=blah]@
  | AttribContains Name Text -- ^ @[attr*=blah]@
  | AttribContainsWord Name Text -- ^ @[attr~=blah]@
  | AttribContainsPrefix Name Text -- ^ @[attr|=blah]@
  deriving (Show, Eq)
