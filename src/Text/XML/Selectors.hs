-- | Description: jQuery/CSS style selectors for xml-conduit
--
-- This module re-exports commonly used functionality from the
-- 'Text.XML.Selectors.Types' and 'Text.XML.Selectors.ToAxis' modules. To parse
-- jQuery selectors, you will also need to import
-- 'Text.XML.Selectors.Parsers.JQ'.
--
-- Basic usage example:
--
-- > import Text.XML.Selectors
-- > import Text.XML.Selectors.Parsers.JQ
-- > import Text.XML as XML
-- > import Text.XML.Cursor (Cursor, node, fromDocument)
-- > import Data.Default
-- > import Control.Monad (forM_)
-- > 
-- > main = do
-- >   doc <- XML.readFile def "example.xml"
-- >   selector <- jqString' "div.menu a[href!='#']"
-- >   let cursors = match selector (fromDocument doc)
-- >   forM_ cursors $ \cursor -> do
-- >     let n = node cursor
-- >     print n
module Text.XML.Selectors
( 
  -- * The Selector Types
  Selector (..)
, (<||>)
, AttribSelector (..)
  -- * Applying Selectors
, toAxis
, match
)
where

import Text.XML.Selectors.Types
import Text.XML.Selectors.ToAxis
