module Solution (Solution(..), S(..)) where

import Control.Monad.Reader

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

instance (Solution m) => Solution (ReaderT String m) where
  getInput = ask
  answer = lift . answer
  answerStr = lift . answerStr
  answerStrLn = lift . answerStrLn

newtype S = S (forall m. (Solution m) => m ())