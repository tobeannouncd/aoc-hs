module Solution (Solution(..), S(..)) where

import Control.Monad.Reader
import Control.Monad.RWS (RWST(..))

class (MonadFail m) => Solution m where
  getInput    :: m String
  answer      :: Show a => a -> m ()
  answer = answerStr . show
  answerStr   :: String -> m ()
  answerStrLn :: String -> m ()
  answerStrLn s = answerStr s >> answerStr "\n"
  {-# MINIMAL getInput, answerStr #-}

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

instance (MonadFail m) => Solution (RWST String ShowS s m) where
  getInput = RWST \inp s -> return (inp,s,mempty)
  answer x = RWST \_ s -> return ((),s,shows x)
  answerStr x = RWST \_ s -> return ((),s,showString x)
  answerStrLn x = RWST \_ s -> return ((),s,showString x . showChar '\n')

newtype S = S (forall m. (Solution m) => m ())