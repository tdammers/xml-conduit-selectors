{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- | Description: Parser for jQuery/CSS selector syntax
--
-- This module provides functions for parsing jQuery/CSS selectors into
-- 'Axis' functions.
--
-- The following selectors are currently supported:
--
-- - __Any selector__ (@*@): Select any node.
-- - __Element name selector__ (@div@): Select all elements of the given
--   element name.
-- - __Descendant selector__ (@a b@): Select /descendants/ of all the nodes
--   matching the LHS selector that match the RHS selector.
-- - __Child selector__ (@a > b@): Select /children/ of all the nodes
--   matching the LHS selector that match the RHS selector.
-- - __Following sibling selector__ (@a ~ b@): Select /siblings/ of all the
--   nodes matching the LHS selector that match the RHS selector.
-- - __Adjacent sibling selector__ (@a + b@):
--   Select /directly adjacent siblings/ of all the nodes matching the LHS
--   selector that match the RHS selector.
-- - __Alternative selector__ (@a , b@): Select all nodes that match the LHS
--   selector, and also all the nodes that match the RHS selector.
-- - __Class selector__ (@.classname@): Select all elements whose @class@
--   attribute contains the given class name.
-- - __ID selector__ (@#id@): Select the element whose @id@ attribute matches
--   the given ID. Note that while in a well-formed XML document, each ID may
--   only be used once, this selector will happily return multiple nodes if the
--   target document is not well-formed, or when it is combined with other
--   selectors that cause the same element to be matched via multiple paths
--   (see also the note below).
-- - __Attribute-exists selector__ (@[attr]@): Select elements that have an
--   attribute of the given name.
-- - __Attribute-is selector__ (@[attr="value"]@): Select elements whose @attr@
--   attribute is exactly equal to the given value.
-- - __Attribute-is-not selector__ (@[attr!="value"]@): Select elements whose
--   @attr@ attribute is not exactly equal to the given value.
-- - __Attribute-starts-with selector__ (@[attr^="value"]@): Select elements whose @attr@
--   attribute begins with the given value.
-- - __Attribute-ends-with selector__ (@[attr$="value"]@): Select elements whose @attr@
--   attribute ends with the given value.
-- - __Attribute-contains selector__ (@[attr*="value"]@): Select elements whose @attr@
--   attribute contains the given value.
-- - __Attribute-contains-word selector__ (@[attr~="value"]@): Select elements
--   whose @attr@ attribute contains the given value between word boundaries.
--   Word boundaries are the very beginning and end of the value, as well as
--   any nonzero number of whitespace characters.
-- - __Attribute-contains-prefix selector__ (@[attr|="value"]@): Select elements whose @attr@
--   attribute contains the given value as a prefix. A prefix is either the
--   entire value itself, or the given value plus a hyphen-minus character
--   (@-@) at the very beginning of the attribute value.
-- - __Not selector__ (@:not(a)@): Select all elements that do not match the
--   argument selector.
-- - __Has selector__ (@:has(a)@): Select all elements that have relatives
--   that match the argument selector. A relationship (@+@, @~@, @>@) can be
--   given at the start of the argument; if none is given, the /descendant/
--   relationship is the default.
-- - __First-child selector__ (@:first-child@): Select all elements that are
--   the first child of their respective parent.
-- - __Last-child selector__ (@:last-child@): Select all elements that are
--   the last child of their respective parent.
-- - __Nth-child selector__ (@:nth-child(n)@): Select all elements that are
--   the n-th child (1-based) of their respective parent.
-- - __Nth-last-child selector__ (@:nth-last-child(n)@): Select all elements
--   that are the n-th last child (1-based) of their respective parent.
--
-- __Note__: The way the 'Axis' and 'Cursor' types work, we cannot avoid
-- matching the same node multiple times - this is mainly because there is no
-- 'Eq' instance for 'Cursor', and thus we cannot remove duplicate matches from
-- the output of an 'Axis'. For example, the element @\<div id="foo"/\>@ would
-- match twice on the selector @div,#foo@: once because it matches the element
-- name @"div"@, and once because it matches the ID @"foo"@.
--
-- Depending on your needs, you may wish to either carefully design your
-- selectors to avoid this, or to cull the results yourself, using a reasonable
-- notion of equality.
module Text.XML.Selectors.Parsers.JQ
( jq
, jqFile
, jqFile'
, jqString
, jqText
, jqString'
, jqText'
, errorBundlePretty
)
where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.XML.Selectors.Types
import Text.XML (Name)
import Data.String (IsString, fromString)
import Data.Char (isAlphaNum, isDigit)

-- | Parse from a stream, error information in 'Left' on failure.
jq :: (IsString (Tokens s), Stream s, Token s ~ Char)
    => String -- ^ Name of source file
    -> s -- ^ Input stream
    -> Either (ParseErrorBundle s Text) Selector
jq = parse jqSelector

-- | Parse from an unnamed 'String' input stream, error information in 'Left' on failure.
jqString :: String -- ^ Input stream
         -> Either (ParseErrorBundle String Text) Selector
jqString = jq "<input>"

-- | Parse from an unnamed 'String' input stream, 'error' on failure.
jqString' :: String -- ^ Input stream
          -> Selector
jqString' = either (error . errorBundlePretty) id . jq "<input>"

-- | Parse from an unnamed 'Text' input stream, error information in 'Left' on failure.
jqText :: Text -- ^ Input stream
       -> Either (ParseErrorBundle Text Text) Selector
jqText = jq "<input>"

-- | Parse from an unnamed 'Text' input stream, 'error' on failure.
jqText' :: Text -- ^ Input stream
        -> Selector
jqText' = either (error . errorBundlePretty) id . jq "<input>"

-- | Parse from a file input stream, error information in 'Left' on failure.
jqFile :: FilePath
       -> IO (Either (ParseErrorBundle Text Text) Selector)
jqFile fn = do
  jq fn <$> Text.readFile fn

-- | Parse from a file input stream, throw a 'UserError' in 'IO' on failure.
jqFile' :: FilePath
        -> IO Selector
jqFile' fn =
  jqFile fn >>= either (fail . errorBundlePretty) return


instance ShowErrorComponent Text where
  showErrorComponent = Text.unpack

jqSelector :: (IsString (Tokens s), Stream s, Token s ~ Char) => Parsec Text s Selector
jqSelector = space *> choices <* space <* eof

choices :: (IsString (Tokens s), Stream s, Token s ~ Char) => Parsec Text s Selector
choices = do
  xs <- item `sepBy` (space *> char ',' <* space)
  case xs of
    [] -> return None
    [x] -> return x
    _ -> return $ Choice xs

item :: (IsString (Tokens s), Stream s, Token s ~ Char) => Parsec Text s Selector
item = do
  what <- option Any (anyElem <|> try elemName)
  qualifiers <- many qualifier
  mcont <- optional continuation
  let self = mconcat (what : qualifiers)
  case mcont of
    Nothing -> pure self
    Just cont -> pure $ self <> cont

anyElem :: (Stream s, Token s ~ Char) => Parsec Text s Selector
anyElem = char '*' *> pure Any

elemName :: (Stream s, Token s ~ Char) => Parsec Text s Selector
elemName = Elem <$> name

name :: (IsString n, Stream s, Token s ~ Char) => Parsec Text s n
name = fromString <$> some nameChar

nameChar :: (Stream s, Token s ~ Char) => Parsec Text s Char
nameChar = satisfy isNameChar

isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || (c `elem` ['_', '-'])

continuation :: (IsString (Tokens s), Stream s, Token s ~ Char) => Parsec Text s Selector
continuation = choice
  [ child
  , sibling
  , nextSibling
  , descendant
  ]

descendant :: (IsString (Tokens s), Stream s, Token s ~ Char) => Parsec Text s Selector
descendant = try space1 *> ((Descendant <>) <$> item)

child :: (IsString (Tokens s), Stream s, Token s ~ Char) => Parsec Text s Selector
child = try (space *> char '>' *> space) *> ((Child <>) <$> item)

sibling :: (IsString (Tokens s), Stream s, Token s ~ Char) => Parsec Text s Selector
sibling = try (space *> char '~' *> space) *> ((Sibling <>) <$> item)

nextSibling :: (IsString (Tokens s), Stream s, Token s ~ Char) => Parsec Text s Selector
nextSibling = try (space *> char '+' *> space) *> ((NextSibling <>) <$> item)

qualifier :: (IsString (Tokens s), Stream s, Token s ~ Char) => Parsec Text s Selector
qualifier = choice
  [ classAttrib
  , idAttrib
  , attribSelector
  , firstChild
  , nthChild
  , nthLastChild
  , having
  , not_
  ]

classAttrib :: (Stream s, Token s ~ Char) => Parsec Text s Selector
classAttrib = do
  char '.'
  value <- name
  pure $ Attrib (AttribContainsWord "class" value)

idAttrib :: (Stream s, Token s ~ Char) => Parsec Text s Selector
idAttrib = do
  char '#'
  value <- name
  pure $ Attrib (AttribIs "id" value)

attribSelector :: (Stream s, Token s ~ Char) => Parsec Text s Selector
attribSelector = do
  char '['
  aname <- name
  mop <- optional attribOp
  asel <- case mop of
    Nothing -> pure $ AttribExists aname
    Just op -> op aname <$> (quotedStr <|> name)
  char ']'
  pure $ Attrib asel

attribOp :: (Stream s, Token s ~ Char) => Parsec Text s (Name -> Text -> AttribSelector)
attribOp = choice
  [ AttribIs <$ try (char '=')
  , AttribIsNot <$ try (char '!' >> char '=')
  , AttribStartsWith <$ try (char '^' >> char '=')
  , AttribEndsWith <$ try (char '$' >> char '=')
  , AttribContains <$ try (char '*' >> char '=')
  , AttribContainsWord <$ try (char '~' >> char '=')
  , AttribContainsPrefix <$ try (char '|' >> char '=')
  ]

quotedStr :: (IsString v, Stream s, Token s ~ Char) => Parsec Text s v
quotedStr = do
  quoteSym <- char '"' <|> char '\''
  val <- many ((char '\\' *> satisfy (const True)) <|> (satisfy (/= quoteSym)))
  _ <- char quoteSym
  pure $ fromString val

firstChild :: (IsString (Tokens s), Stream s, Token s ~ Char) => Parsec Text s Selector
firstChild = FirstChild <$ try (string ":first-child") <* notFollowedBy nameChar

lastChild :: (IsString (Tokens s), Stream s, Token s ~ Char) => Parsec Text s Selector
lastChild = LastChild <$ try (string ":last-child") <* notFollowedBy nameChar

nthChild :: (IsString (Tokens s), Stream s, Token s ~ Char) => Parsec Text s Selector
nthChild = do
  NthChild <$> (try (string ":nth-child") *> char '(' *> space *> positiveInt <* space <* char ')')

nthLastChild :: (IsString (Tokens s), Stream s, Token s ~ Char) => Parsec Text s Selector
nthLastChild = do
  NthChild . negate <$> (try (string ":nth-last-child") *> char '(' *> space *> positiveInt <* space <* char ')')

positiveInt :: (Stream s, Token s ~ Char) => Parsec Text s Int
positiveInt = read <$> some digitChar

having :: (IsString (Tokens s), Stream s, Token s ~ Char) => Parsec Text s Selector
having = try (string ":has") *> space *> char '(' *> (Having <$> item) <* char ')'

not_ :: (IsString (Tokens s), Stream s, Token s ~ Char) => Parsec Text s Selector
not_ = try (string ":not") *> space *> char '(' *> (Not <$> item) <* char ')'
