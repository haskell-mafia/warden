{-# LANGUAGE LambdaCase #-}

import           Options.Applicative
import           System.Exit

import           X.Options.Applicative

data Command = Check FilePath
  deriving (Eq, Show)

main :: IO ()
main = do
  dispatch parse >>= \case
    VersionCommand -> do
      exitSuccess
    RunCommand DryRun c -> do
      print c
      exitSuccess
    RunCommand RealRun _ -> do
      putStrLn "nyi"
      exitSuccess

parse :: Parser (SafeCommand Command)
parse = safeCommand (Check <$> view)

view :: Parser FilePath
view = strArgument $
     metavar "VIEW"
  <> help "Path to view."
