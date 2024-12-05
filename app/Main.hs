{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Time
import Data.Text (pack)
import Options.Applicative
import Turtle (stdout, inshell, input, stdin, Text, toLines)
import Text.Printf (printf)
import System.Environment (getEnv, lookupEnv)
import Advent

main :: IO ()
main = do
  (year, day, runType) <- execParser =<< mkParser
  let fp = getSolution year day
  case runType of
    RDownload -> stdout . inshell fp . toLines . pure =<< getInput year day
    RStdin    -> stdout $ inshell fp stdin
    RFile inp -> stdout $ inshell fp (input inp)

getSolution :: Integer -> Integer -> Text
getSolution y d = pack $ printf "stack runghc ./src/Y%d/D%02d.hs" y d

getInput :: Integer -> Integer -> IO Text
getInput year day = do
  session <- getEnv "AOC_SESSION"
  cache <- lookupEnv "AOC_CACHE"
  day' <- maybe (fail $ "invalid day: " ++ show day) return (mkDay day)
  let agent = AoCUserAgent "tobeannouncd/advent-haskell" "tobeannouncd@gmail.com"
      opts = (defaultAoCOpts agent year session) {_aCache = cache}
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