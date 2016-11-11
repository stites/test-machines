module Main where

import qualified Data.Text as T
import Options.Applicative
import Prelude

import EvenProcess
import Test.Machines

data Api = Api
  { count :: Integer
  , filename :: FilePath
  , delim :: String
  -- , out   :: Bool
  }

countsApi :: Parser Api
countsApi = Api
  <$> argument auto
      ( metavar "n"
     <> help "number of events to generate" )
  <*> strOption
      ( long "file"
     <> short 'f'
     <> metavar "BASENAME"
     <> value "even"
     <> help "base name to generate files for" )
  <*> strOption
      ( long "delim"
     <> metavar "STR"
     <> value ""
     <> help "delimiter for events and states in generated files" )
  -- <*> switch
  --     ( long "out"
  --    <> help "Whether to stream to stdout" )


main :: IO ()
main = execParser opts >>= greet
  where
    opts = info (helper <*> countsApi)
      ( fullDesc
     <> progDesc "Generate a stoicastic series of events from an even process"
     <> header "even-process" )

    greet :: Api -> IO ()
    greet (Api n file d) = writeSeries evenProxy (T.pack d) file n

