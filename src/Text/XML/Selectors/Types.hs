module Text.XML.Selectors.Types
where

import Text.XML
import Text.XML.Cursor
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Applicative

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
  | NthChild Int -- ^ @:nth-child(n)@; @:nth-last-child(-n)@
  | Choice [Selector] -- ^ @a,b,...@
  | Having Selector -- ^ @:has(b)@
  | Not Selector -- ^ @:not(b)@
  deriving (Show)

selectorAppend :: Selector -> Selector -> Selector
selectorAppend Any x = x
selectorAppend x Any = x
selectorAppend a b = Append a b

instance Semigroup Selector where
  (<>) = selectorAppend

instance Monoid Selector where
  mappend = (<>)
  mempty = Any

(<||>) :: Selector -> Selector -> Selector
Choice xs <||> Choice ys = Choice (xs ++ ys)
Choice xs <||> y = Choice (xs ++ [y])
x <||> Choice ys = Choice (x : ys)
x <||> y = Choice [x, y]
infixl 3 <||>

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
