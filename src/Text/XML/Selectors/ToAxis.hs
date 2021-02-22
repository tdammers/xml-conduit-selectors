{-#LANGUAGE OverloadedStrings #-}
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

match :: Selector -> Cursor -> [Cursor]
match selector root =
  removeDoubles . (orSelf descendant >=> toAxis selector) $ root

removeDoubles :: [Cursor] -> [Cursor]
removeDoubles = nubBy isSameCursor

isSameCursor :: Cursor -> Cursor -> Bool
isSameCursor a b = cursorPath a == cursorPath b

cursorPath :: Cursor -> [Node]
cursorPath c =
  node c : map node (ancestor c)
