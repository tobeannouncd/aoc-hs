module Y2024.D03 (main) where

import AoC.Parsec
import Solution

data Command
  = Mul !Int
  | Enable | Disable | Other

parseCommands :: MonadFail m => String -> m [Command]
parseCommands = either (fail . show) return . parse (many cmd) ""
 where
  cmd = try mul <|> try enable <|> try disable <|> (Other <$ anyChar)
  mul = do
    a <- string "mul(" *> nat
    b <- char ',' *> nat <* char ')'
    return $ Mul (a*b)
  enable = Enable <$ string "do()"
  disable = Disable <$ string "don't()"

main :: Solution m => m ()
main = do
  inp <- parseCommands =<< getInput
  answer $ sum [a | Mul a <- inp]
  answer $ fst $ foldl part2 (0, True) inp

part2 :: (Int, Bool) -> Command -> (Int, Bool)
part2 (s, p) (Mul a) = (s + a*fromEnum p, p)
part2 (s, _) Enable = (s, True)
part2 (s, _) Disable = (s, False)
part2 acc _ = acc