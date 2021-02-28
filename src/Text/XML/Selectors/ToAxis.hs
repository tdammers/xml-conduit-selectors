{-#LANGUAGE OverloadedStrings #-}

-- | Description: Convert 'Selector's into 'Axis' functions.
module Text.XML.Selectors.ToAxis
where

import Text.XML
import Text.XML.Cursor
import Text.XML.Selectors.Types
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.List (nubBy)

-- | Turn a 'Selector' into an 'Axis'.
toAxis :: Selector -> Axis
-- @*@
toAxis Any =
  (:[])
toAxis None =
  const []
toAxis (Append a b) =
  toAxis a >=> toAxis b
-- @div@
toAxis (Elem name) =
  element name
-- @a[...]@
toAxis (Attrib p) =
  checkAttrib p
-- @a b@
toAxis Descendant =
  descendant
-- @a>b@
toAxis Child =
  child
-- @a~b@
toAxis Sibling =
  followingSibling
-- @a+b@
toAxis NextSibling =
  take 1 . followingSibling
-- @:first-child@
toAxis FirstChild =
  check (null . precedingSibling)
-- @:last-child@
toAxis LastChild =
  check (null . followingSibling)
-- @:nth-child(n)@; @:nth-last-child(-n)@
toAxis (NthChild i)
  | i > 0 = check ((== i - 1) . length . precedingSibling)
  | i < 0 = check ((== (-i) - 1) . length . followingSibling)
  | otherwise = error ":nth-child(0)"
-- @a,b,...@
toAxis (Choice xs) =
  \c -> concatMap (\x -> toAxis x c) xs
-- @a:has(b)@
toAxis (Having s) =
  check (descendant >=> toAxis s)
-- @a:not(b)@
toAxis (Not s) =
  check (null . toAxis s)

-- | Directly apply a 'Selector' to a 'Cursor', removing duplicates. Cursors
-- are considered duplicate iff their focus node /and/ ancestory are the same.
--
-- Due to the knot-tying of the 'Cursor' type, this is not perfect: we are not
-- considering the focus node's position within its parent, so any two nodes
-- that are exactly identical themselves and share ancestory will be considered
-- equal. E.g., in the following XML document:
--
-- > <root>
-- >   <parent>
-- >      <child>Foo</child>
-- >      <child>Foo</child>
-- >   </parent>
-- > </root>
--
-- ...the two @\<child/\>@ nodes will be considered equal, even though they are
-- two distinct nodes in the DOM.
--
-- Unlike 'toAxis', the 'match' function prepends an implicit
-- self-or-descendant 'Axis' to the selector in order to mimic the behavior of
-- actual CSS selectors.
match :: Selector -> Cursor -> [Cursor]
match selector root =
  removeDoubles . (orSelf descendant >=> toAxis selector) $ root

checkAttrib :: AttribSelector -> Axis
checkAttrib asel = checkElement (checkElementAttribs asel . elementAttributes)

checkElementAttribs :: AttribSelector -> Map Name Text -> Bool
-- @[attr]@
checkElementAttribs (AttribExists n) attrs =
  Map.member n attrs
-- @[attr=blah]@
checkElementAttribs (AttribIs n v) attrs =
  Map.lookup n attrs == Just v
-- @[attr!=blah]@
checkElementAttribs (AttribIsNot n v) attrs =
  Map.lookup n attrs /= Just v
-- @[attr^=blah]@
checkElementAttribs (AttribStartsWith n v) attrs =
  case Map.lookup n attrs of
    Just t -> v `Text.isPrefixOf` t
    Nothing -> False
-- @[attr$=blah]@
checkElementAttribs (AttribEndsWith n v) attrs =
  case Map.lookup n attrs of
    Just t -> v `Text.isSuffixOf` t
    Nothing -> False
-- @[attr*=blah]@
checkElementAttribs (AttribContains n v) attrs =
  case Map.lookup n attrs of
    Just t -> v `Text.isInfixOf` t
    Nothing -> False
-- @[attr~=blah]@
checkElementAttribs (AttribContainsWord n v) attrs =
  case Map.lookup n attrs of
    Just t -> v `elem` Text.words t
    Nothing -> False
-- @[attr|=blah]@
checkElementAttribs (AttribContainsPrefix n v) attrs =
  case Map.lookup n attrs of
    Just t -> t == v || (v <> "-") `Text.isPrefixOf` t
    Nothing -> False

removeDoubles :: [Cursor] -> [Cursor]
removeDoubles = nubBy isSameCursor

isSameCursor :: Cursor -> Cursor -> Bool
isSameCursor a b = cursorPath a == cursorPath b

cursorPath :: Cursor -> [Node]
cursorPath c =
  node c : map node (ancestor c)
