module Filtration where

import Data.List (foldl')

buildFiltration distance points maxDim maxDist = foldl' (<>) mempty (pure <$> points)
