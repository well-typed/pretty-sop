## pretty-sop

[![Build Status](https://travis-ci.org/well-typed/pretty-sop.svg?branch=master)](https://travis-ci.org/well-typed/pretty-sop)
[![Hackage](https://img.shields.io/hackage/v/pretty-sop.svg)](https://hackage.haskell.org/package/pretty-sop)

This library contains a generic implementation of the `prettyVal` function
from the [pretty-show](https://hackage.haskell.org/package/pretty-show) package.
Using the pretty printer, values can easily be rendered to strings and HTML
documents in a uniform way.

This library makes use of the [generics-sop](https://github.com/well-typed/generics-sop)
package and is an example of a generic function defined in the SOP style.

This is the development repository. For releases, look on
[Hackage](https://hackage.haskell.org/package/pretty-sop).


## Example

```haskell
{-# LANGUAGE DeriveGeneric #-}

import Generics.SOP.PrettyVal
import Generics.SOP
import qualified GHC.Generics as G
import Text.Show.Pretty


data Car = Car
  { name :: String
  , seats :: Int
  } deriving (G.Generic)

instance Generic Car
instance HasDatatypeInfo Car
instance PrettyVal Car where
  prettyVal = gprettyVal

main :: IO ()
main = putStrLn $ dumpStr $ Car "test" 3
```

would print:

```text
Car { name = test , seats = 3 }
```
