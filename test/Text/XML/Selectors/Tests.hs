{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
module Text.XML.Selectors.Tests
where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import System.IO.Unsafe (unsafePerformIO)
import Data.Default
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import Data.Map (Map)
import qualified Data.Map as Map

import Text.XML
import Text.XML.Cursor
import Text.XML.Selectors
import Text.XML.Selectors.ToAxis

tests :: TestTree
tests =
  testGroup "selectors"
    [ testGroup "example-tests"
      [ testCase "name" $
          testSelector
            (elemNode "root" []
              [ elemNode "foo" [] [ textNode "hello" ]
              , elemNode "bar" [] []
              , elemNode "bar" [] []
              , elemNode "foo" [] [ textNode "world" ]
              ]
            )
            (Elem "foo")
            [ elemNode "foo" [] [ textNode "hello" ]
            , elemNode "foo" [] [ textNode "world" ]
            ]
      , testCase "attrib-is" $
          testSelector
            (elemNode "root" []
              [ elemNode "foo" [("id", "blub")] [ textNode "hello" ]
              , elemNode "foo" [("id", "blah")] [ textNode "world" ]
              , elemNode "foo" [("id", "pizza")] [ textNode "moon" ]
              , elemNode "foo" [("ident", "pasta")] [ textNode "mars" ]
              ]
            )
            (ByAttrib $ AttribIs "id" "blah")
            [ elemNode "foo" [("id", "blah")] [ textNode "world" ]
            ]
      , testCase "attrib-exists" $
          testSelector
            (elemNode "root" []
              [ elemNode "foo" [("id", "blub")] [ textNode "hello" ]
              , elemNode "foo" [("id", "blah")] [ textNode "world" ]
              , elemNode "foo" [("id", "pizza")] [ textNode "moon" ]
              , elemNode "foo" [("ident", "pasta")] [ textNode "mars" ]
              ]
            )
            (ByAttrib $ AttribExists "id")
            [ elemNode "foo" [("id", "blub")] [ textNode "hello" ]
            , elemNode "foo" [("id", "blah")] [ textNode "world" ]
            , elemNode "foo" [("id", "pizza")] [ textNode "moon" ]
            ]
      , testCase "attrib-is-not" $
          testSelector
            (elemNode "root" []
              [ elemNode "foo" [("id", "blub")] [ textNode "hello" ]
              , elemNode "foo" [("id", "blah")] [ textNode "world" ]
              , elemNode "foo" [("id", "pizza")] [ textNode "moon" ]
              , elemNode "foo" [("ident", "pasta")] [ textNode "mars" ]
              ]
            )
            (Elem "foo" <> ByAttrib (AttribIsNot "id" "pizza"))
            [ elemNode "foo" [("id", "blub")] [ textNode "hello" ]
            , elemNode "foo" [("id", "blah")] [ textNode "world" ]
            , elemNode "foo" [("ident", "pasta")] [ textNode "mars" ]
            ]
      , testCase "attrib-starts-with" $
          testSelector
            (elemNode "root" []
              [ elemNode "foo" [("id", "blub")] [ textNode "hello" ]
              , elemNode "foo" [("id", "blah")] [ textNode "world" ]
              , elemNode "foo" [("id", "pizza")] [ textNode "moon" ]
              , elemNode "foo" [("ident", "pasta")] [ textNode "mars" ]
              ]
            )
            (Elem "foo" <> ByAttrib (AttribStartsWith "id" "bl"))
            [ elemNode "foo" [("id", "blub")] [ textNode "hello" ]
            , elemNode "foo" [("id", "blah")] [ textNode "world" ]
            ]
      , testCase "attrib-ends-with" $
          testSelector
            (elemNode "root" []
              [ elemNode "foo" [("id", "blub")] [ textNode "hello" ]
              , elemNode "foo" [("id", "blah")] [ textNode "world" ]
              , elemNode "foo" [("id", "pizza")] [ textNode "moon" ]
              , elemNode "foo" [("id", "pasta")] [ textNode "mars" ]
              ]
            )
            (Elem "foo" <> ByAttrib (AttribEndsWith "id" "a"))
            [ elemNode "foo" [("id", "pizza")] [ textNode "moon" ]
            , elemNode "foo" [("id", "pasta")] [ textNode "mars" ]
            ]
      , testCase "attrib-contains" $
          testSelector
            (elemNode "root" []
              [ elemNode "foo" [("id", "blub")] [ textNode "hello" ]
              , elemNode "foo" [("id", "blah")] [ textNode "world" ]
              , elemNode "foo" [("id", "pizza")] [ textNode "moon" ]
              , elemNode "foo" [("id", "pasta")] [ textNode "mars" ]
              ]
            )
            (Elem "foo" <> ByAttrib (AttribContains "id" "a"))
            [ elemNode "foo" [("id", "blah")] [ textNode "world" ]
            , elemNode "foo" [("id", "pizza")] [ textNode "moon" ]
            , elemNode "foo" [("id", "pasta")] [ textNode "mars" ]
            ]
      , testCase "attrib-contains-word" $
          testSelector
            (elemNode "root" []
              [ elemNode "foo" [("id", "blub")] [ textNode "hello" ]
              , elemNode "foo" [("id", "party-blah")] [ textNode "world" ]
              , elemNode "foo" [("id", "pizza party")] [ textNode "moon" ]
              , elemNode "foo" [("id", "party pasta")] [ textNode "mars" ]
              ]
            )
            (Elem "foo" <> ByAttrib (AttribContainsWord "id" "party"))
            [ elemNode "foo" [("id", "pizza party")] [ textNode "moon" ]
            , elemNode "foo" [("id", "party pasta")] [ textNode "mars" ]
            ]
      , testCase "attrib-contains-prefix" $
          testSelector
            (elemNode "root" []
              [ elemNode "foo" [("id", "blub")] [ textNode "hello" ]
              , elemNode "foo" [("id", "party-blah")] [ textNode "world" ]
              , elemNode "foo" [("id", "party")] [ textNode "moon" ]
              , elemNode "foo" [("id", "party pasta")] [ textNode "mars" ]
              ]
            )
            (Elem "foo" <> ByAttrib (AttribContainsPrefix "id" "party"))
            [ elemNode "foo" [("id", "party-blah")] [ textNode "world" ]
            , elemNode "foo" [("id", "party")] [ textNode "moon" ]
            ]
      , testCase "descendant" $
          testSelector
            (elemNode "root" []
              [ elemNode "foo" []
                  [ elemNode "bar" []
                     [ textNode "hello" ]
                  ]
              , elemNode "foo" []
                  [ elemNode "blah" []
                    [ elemNode "bar" []
                       [ textNode "hola" ]
                    ]
                  ]
              , elemNode "bar" []
                  [ textNode "goodbye" ]
              ]
            )
            (Elem "foo" <> Descendant <> Elem "bar")
            [ elemNode "bar" [] [ textNode "hello" ]
            , elemNode "bar" [] [ textNode "hola" ]
            ]
      , testCase "child" $
          testSelector
            (elemNode "root" []
              [ elemNode "foo" []
                  [ elemNode "bar" []
                     [ textNode "hello" ]
                  ]
              , elemNode "foo" []
                  [ elemNode "blah" []
                    [ elemNode "bar" []
                       [ textNode "hola" ]
                    ]
                  ]
              , elemNode "bar" []
                  [ textNode "goodbye" ]
              ]
            )
            (Elem "foo" <> Child <> Elem "bar")
            [ elemNode "bar" [] [ textNode "hello" ]
            ]
      , testCase "sibling" $
          testSelector
            (elemNode "root" []
              [ elemNode "foo" [] []
              , elemNode "bar" [] [ textNode "hello" ]
              , elemNode "foo" [] []
              , elemNode "blah" [] []
              , elemNode "bar" [] [ textNode "hola" ]
              , elemNode "foo" []
                [ elemNode "bar" [] [ textNode "goodbye" ]
                ]
              ]
            )
            (Elem "foo" <> Sibling <> Elem "bar")
            [ elemNode "bar" [] [ textNode "hello" ]
            , elemNode "bar" [] [ textNode "hola" ]
            ]
      , testCase "next-sibling" $
          testSelector
            (elemNode "root" []
              [ elemNode "foo" [] []
              , elemNode "bar" [] [ textNode "hello" ]
              , elemNode "foo" [] []
              , elemNode "blah" [] []
              , elemNode "bar" [] [ textNode "hola" ]
              , elemNode "foo" []
                [ elemNode "bar" [] [ textNode "goodbye" ]
                ]
              ]
            )
            (Elem "foo" <> NextSibling <> Elem "bar")
            [ elemNode "bar" [] [ textNode "hello" ]
            ]
      , testCase "first-child" $
          testSelector
            (elemNode "root" []
              [ elemNode "foo" []
                [ elemNode "bar" [] [ textNode "hello" ]
                , elemNode "bar" [] [ textNode "goodbye" ]
                , elemNode "bar" [] [ textNode "hola" ]
                ]
              ]
            )
            (Elem "foo" <> Child <> FirstChild)
            [ elemNode "bar" [] [ textNode "hello" ]
            ]
      , testCase "last-child" $
          testSelector
            (elemNode "root" []
              [ elemNode "foo" []
                [ elemNode "bar" [] [ textNode "hello" ]
                , elemNode "bar" [] [ textNode "goodbye" ]
                , elemNode "bar" [] [ textNode "hola" ]
                ]
              ]
            )
            (Elem "foo" <> Child <> LastChild)
            [ elemNode "bar" [] [ textNode "hola" ]
            ]
      , testCase "nth-child" $
          testSelector
            (elemNode "root" []
              [ elemNode "foo" []
                [ elemNode "bar" [] [ textNode "hello" ]
                , elemNode "bar" [] [ textNode "goodbye" ]
                , elemNode "bar" [] [ textNode "sayonara" ]
                , elemNode "bar" [] [ textNode "hola" ]
                ]
              ]
            )
            (Elem "foo" <> Child <> NthChild 2)
            [ elemNode "bar" [] [ textNode "goodbye" ]
            ]
      , testCase "nth-last-child" $
          testSelector
            (elemNode "root" []
              [ elemNode "foo" []
                [ elemNode "bar" [] [ textNode "hello" ]
                , elemNode "bar" [] [ textNode "goodbye" ]
                , elemNode "bar" [] [ textNode "sayonara" ]
                , elemNode "bar" [] [ textNode "hola" ]
                ]
              ]
            )
            (Elem "foo" <> Child <> NthChild (-2))
            [ elemNode "bar" [] [ textNode "sayonara" ]
            ]
      , testCase "choice" $
          testSelector
            (elemNode "root" []
              [ elemNode "foo" [] [ textNode "hello" ]
              , elemNode "bar" [] [ textNode "goodbye" ]
              , elemNode "baz" [] [ textNode "sayonara" ]
              , elemNode "quux" [] [ textNode "hola" ]
              ]
            )
            (Choice [Elem "bar", Elem "baz"])
            [ elemNode "bar" [] [ textNode "goodbye" ]
            , elemNode "baz" [] [ textNode "sayonara" ]
            ]
      , testCase "choice operator (<||>)" $
          testSelector
            (elemNode "root" []
              [ elemNode "foo" [] [ textNode "hello" ]
              , elemNode "bar" [] [ textNode "goodbye" ]
              , elemNode "baz" [] [ textNode "sayonara" ]
              , elemNode "quux" [] [ textNode "hola" ]
              ]
            )
            (Elem "bar" <||> Elem "baz")
            [ elemNode "bar" [] [ textNode "goodbye" ]
            , elemNode "baz" [] [ textNode "sayonara" ]
            ]
      , testCase "having" $
          testSelector
            (elemNode "root" []
              [ elemNode "foo" []
                  [ elemNode "bar" [] []
                  ]
              , elemNode "foo" [] []
              , elemNode "baz" []
                  [ elemNode "bar" [] []
                  ]
              , elemNode "foo" []
                  [ elemNode "quux" [] []
                  ]
              ]
            )
            (Elem "foo" <> Having (Descendant <> Elem "bar"))
            [ elemNode "foo" [] [ elemNode "bar" [] [] ]
            ]
      ]
    ]

testSelector :: Node -> Selector -> [Node] -> Assertion
testSelector root selector expected = do
  let actual =
        match selector .
        fromDocument .
        wrapDocument $
        root
  assertEqual "" (PrettyNodes expected) (PrettyNodes . map node $ actual)

nodeContent :: Node -> Text
nodeContent (NodeContent t) = t
nodeContent (NodeElement (Element _ _ children)) = mconcat . map nodeContent $ children
nodeContent _ = ""

renderFragmentText :: Node -> LText.Text
renderFragmentText = renderText def { rsXMLDeclaration = False } . wrapDocument

wrapDocument :: Node -> Document
wrapDocument (NodeElement e) =
    Document
      { documentPrologue = Prologue [] Nothing []
      , documentRoot = e
      , documentEpilogue = []
      }
wrapDocument n =
  wrapDocument
    (elemNode "root" [] [n])

elemNode :: Name -> Map Name Text -> [Node] -> Node
elemNode name attr children = NodeElement $ Element name attr children

textNode :: Text -> Node
textNode = NodeContent

newtype PrettyNodes = PrettyNodes [Node]
  deriving (Eq)

instance Show PrettyNodes where
  show (PrettyNodes n) =
    unlines . map (LText.unpack . renderFragmentText) $ n
