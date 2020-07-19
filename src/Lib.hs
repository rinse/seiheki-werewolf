module Lib
    ( someFunc
    ) where

{- |It returns someFunc.

    >>> someFunc
    "someFunc"

    prop> const someFunc x == someFunc
-}
someFunc :: String
someFunc = "someFunc"
