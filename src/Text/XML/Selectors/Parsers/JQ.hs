{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Text.XML.Selectors.Parsers.JQ
( jqFile
, jqFile'
, jqStr
, jqStr'
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

_jq :: (IsString (Tokens s), Stream s, Token s ~ Char)
    => String -- ^ Name of source file
    -> s -- ^ Input stream
    -> Either (ParseErrorBundle s Text) Selector
_jq = parse jqSelector

_jq' :: String -- ^ Name of source file
     -> String -- ^ Input stream
     -> Selector
_jq' fn s = either (error . errorBundlePretty) id $ _jq fn s

jqStr :: (IsString (Tokens s), Stream s, Token s ~ Char)
      => s -- ^ Input stream
      -> Either (ParseErrorBundle s Text) Selector
jqStr = _jq "<input>"

jqStr' :: String -- ^ Input stream
       -> Selector
jqStr' = _jq' "<input>"

jqFile :: FilePath
       -> IO (Either (ParseErrorBundle Text Text) Selector)
jqFile fn = do
  _jq fn <$> Text.readFile fn

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

not :: (IsString (Tokens s), Stream s, Token s ~ Char) => Parsec Text s Selector
not = try (string ":not") *> space *> char '(' *> (Not <$> item) <* char ')'
