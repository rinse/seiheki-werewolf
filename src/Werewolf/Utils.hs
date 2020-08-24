module Werewolf.Utils where

import           Control.Monad              (join)
import           Control.Monad.Random.Class (MonadRandom)
import           Data.Function              (on)
import           Data.List                  (groupBy, sortBy, transpose)
import           System.Random.Shuffle      (shuffleM)


{-|Groups a given List by a Function and shuffle elements of each group.
>>> import Control.Monad.Random
>>> import System.Random
>>> :{
    let gen = mkStdGen 0
        input = [("en","Hello"),("fr","Bonjour"),("sp","Hola"),("en","Good bye"),("fr","Au revoir"),("sp","Hasta la vista")]
     in runRand (groupedShuffle fst input) gen
    :}
([("en","Good bye"),("fr","Au revoir"),("sp","Hasta la vista"),("fr","Bonjour"),("sp","Hola"),("en","Hello")],1962667596 535353314)
-}
groupedShuffle :: (MonadRandom m, Ord b) => (a -> b) -> [a] -> m [a]
groupedShuffle f list = do
    let sorted = sortBy (compare `on` f) list
        grouped = groupBy ((==) `on` f) sorted
    fmap join $ traverse shuffleM grouped >>= traverse shuffleM . transpose
