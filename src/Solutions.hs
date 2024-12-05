module Solutions (solutions) where

import Solution (S(..))
import Data.Map.Strict (Map, fromList)

import Y2024.D01 qualified as Y24D01
import Y2024.D02 qualified as Y24D02
import Y2024.D03 qualified as Y24D03
import Y2024.D04 qualified as Y24D04
import Y2024.D05 qualified as Y24D05
import Y2024.D06 qualified as Y24D06

solutions :: Map (Integer,Integer) S
solutions = fromList
  [ ((2024,1), S Y24D01.main)
  , ((2024,2), S Y24D02.main)
  , ((2024,3), S Y24D03.main)
  , ((2024,4), S Y24D04.main)
  , ((2024,5), S Y24D05.main)
  , ((2024,6), S Y24D06.main)
  ]