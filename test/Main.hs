module Main where

import Test.Tasty (defaultMain, TestTree (..), testGroup)

import qualified Text.XML.Selectors.Tests as SelectorsTests

tests :: TestTree
tests =
  testGroup "xml-conduit-selectors"
    [ SelectorsTests.tests
    ]

main = defaultMain tests
