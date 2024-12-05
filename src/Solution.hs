module Solution (Solution(..)) where

class (MonadFail m) => Solution m where
  getInput    :: m String
  answer      :: Show a => a -> m ()
  answerStr   :: String -> m ()
  answerStrLn :: String -> m ()
  answerStrLn s = answerStr s >> answerStr "\n"
  {-# MINIMAL getInput, answer, answerStr #-}

instance Solution IO where
  getInput = getContents
  answer = print
  answerStr = putStr
  answerStrLn = putStrLn