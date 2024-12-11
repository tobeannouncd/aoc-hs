{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Advent
import Control.Monad.Reader (ReaderT (..))
import Data.Map.Strict ((!?))
import Data.Text (Text, unpack)
import Data.Time
import Options.Applicative
import Solution (S (..))
import Solutions (solutions)
import System.Environment (getEnv, lookupEnv)
import Control.Monad.RWS (RWST (runRWST))

main :: IO ()
main = do
  (year, day, runType) <- execParser =<< mkParser
  soln <- getSolution year day
  case runType of
    RDownload -> runDownloaded soln . unpack =<< getInput year day
    RStdin    -> runIO soln
    RFile f   -> runDownloaded soln =<< readFile f

runDownloaded :: S -> String -> IO ()
runDownloaded (S soln) = runReaderT soln

runIO :: S -> IO ()
runIO (S soln) = soln

runMaybe :: S -> String -> Maybe String
runMaybe (S soln) inp = (\(_,_,x) -> x "")
                      <$> (runRWST soln inp () :: Maybe ((),(),ShowS))

getSolution :: Integer -> Integer -> IO S
getSolution y d =
  case solutions !? (y, d) of
    Nothing -> fail "solution not found"
    Just s -> return s

getInput :: Integer -> Integer -> IO Text
getInput year day = do
  session <- getEnv "AOC_SESSION"
  cache <- lookupEnv "AOC_CACHE"
  day' <- maybe (fail $ "invalid day: " ++ show day) return (mkDay day)
  let agent = AoCUserAgent "tobeannouncd/aoc-hs" "tobeannouncd@gmail.com"
      opts = (defaultAoCOpts agent year session){_aCache = cache}
  runAoC_ opts $ AoCInput day'

latest :: IO (Integer, Integer)
latest = do
  let est = hoursToTimeZone (-5)
  (yy, mm, dd) <- toGregorian . localDay . utcToLocalTime est <$> getCurrentTime
  return $
    if mm == 12
      then (yy, min 25 (toInteger dd))
      else (yy - 1, 25)

data RunType
  = RDownload
  | RStdin
  | RFile FilePath

mkParser :: IO (ParserInfo (Integer, Integer, RunType))
mkParser = do
  (yearDef, dayDef) <- latest
  let parser = p yearDef dayDef
  return $ info (parser <**> helper) mods
 where
  mods = mempty
  p y d =
    (,,)
      <$> argument
        auto
        ( metavar "YEAR"
            <> value y
            <> showDefault
            <> help "puzzle year"
        )
      <*> argument
        auto
        ( metavar "DAY"
            <> value d
            <> showDefault
            <> help "puzzle day"
        )
      <*> ( flag' RStdin (short 's' <> long "stdin" <> help "use stdin as input")
              <|> RFile
              <$> strOption
                ( short 'f'
                    <> long "file"
                    <> completer (bashCompleter "directory")
                    <> help "get input from file"
                )
                <|> pure RDownload
          )