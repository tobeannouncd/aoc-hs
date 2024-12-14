module Y2024.Solutions (solutions) where

import Solution(S(..))
import Data.Map.Strict (Map, fromList)

import Y2024.D01 qualified as Y24D01
import Y2024.D02 qualified as Y24D02
import Y2024.D03 qualified as Y24D03
import Y2024.D04 qualified as Y24D04
import Y2024.D05 qualified as Y24D05
import Y2024.D06 qualified as Y24D06
import Y2024.D07 qualified as Y24D07
import Y2024.D08 qualified as Y24D08
import Y2024.D09 qualified as Y24D09
import Y2024.D10 qualified as Y24D10
import Y2024.D11 qualified as Y24D11
import Y2024.D12 qualified as Y24D12
import Y2024.D13 qualified as Y24D13
import Y2024.D14 qualified as Y24D14
import Y2024.D15 qualified as Y24D15
import Y2024.D16 qualified as Y24D16
import Y2024.D17 qualified as Y24D17
import Y2024.D18 qualified as Y24D18
import Y2024.D19 qualified as Y24D19
import Y2024.D20 qualified as Y24D20
import Y2024.D21 qualified as Y24D21
import Y2024.D22 qualified as Y24D22
import Y2024.D23 qualified as Y24D23
import Y2024.D24 qualified as Y24D24
import Y2024.D25 qualified as Y24D25

solutions :: Map (Integer,Integer) S
solutions = fromList
  [ ((2024,1), S Y24D01.main)
  , ((2024,2), S Y24D02.main)
  , ((2024,3), S Y24D03.main)
  , ((2024,4), S Y24D04.main)
  , ((2024,5), S Y24D05.main)
  , ((2024,6), S Y24D06.main)
  , ((2024,7), S Y24D07.main)
  , ((2024,8), S Y24D08.main)
  , ((2024,9), S Y24D09.main)
  , ((2024,10), S Y24D10.main)
  , ((2024,11), S Y24D11.main)
  , ((2024,12), S Y24D12.main)
  , ((2024,13), S Y24D13.main)
  , ((2024,14), S Y24D14.main)
  , ((2024,15), S Y24D15.main)
  , ((2024,16), S Y24D16.main)
  , ((2024,17), S Y24D17.main)
  , ((2024,18), S Y24D18.main)
  , ((2024,19), S Y24D19.main)
  , ((2024,20), S Y24D20.main)
  , ((2024,21), S Y24D21.main)
  , ((2024,22), S Y24D22.main)
  , ((2024,23), S Y24D23.main)
  , ((2024,24), S Y24D24.main)
  , ((2024,25), S Y24D25.main)
  ]