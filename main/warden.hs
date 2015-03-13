import           Data.Text (Text, pack)

import           Warden

import           Options.Applicative

import           System.Environment
import           System.Exit

import           Text.Read (readMaybe)

data Command =
  Command {
    } deriving (Eq, Show)

main :: IO ()
main =
  execParser (info commandParser idm) >>= \c -> do
    putStrLn "TODO"

commandParser :: Parser Command
commandParser =
  pure Command
