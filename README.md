# xml-conduit-selectors

jQuery-style CSS selectors for xml-conduit

## Introduction

The [`xml-conduit` library](https://hackage.haskell.org/package/xml-conduit)
provides the `Axis` type alias, which represents "node selection" in an XML
document. All sorts of primitives and combinators are provided alongside these,
implementing a powerful selection and filtering EDSL. This library,
`xml-conduit-selectors`, adds a jQuery/CSS selector layer on top of this,
allowing you to define `Axis` functions using CSS selector syntax.
