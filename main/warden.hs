import           Options.Applicative

import           System.Exit

data Command =
  Command {
    } deriving (Eq, Show)

main :: IO ()
main =
  execParser (info commandParser idm) >>= \_ -> do
    putStrLn "TODO" >> exitFailure

commandParser :: Parser Command
commandParser =
  pure Command
