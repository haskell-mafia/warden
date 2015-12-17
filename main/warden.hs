{-# LANGUAGE LambdaCase #-}

import qualified Data.List.NonEmpty as NE
import           Data.Text (Text)
import qualified Data.Text.IO as T

import           Options.Applicative
import           System.Exit

import           Warden.Commands
import           Warden.Data
import           Warden.Error

import           X.Control.Monad.Trans.Either (EitherT)
import           X.Control.Monad.Trans.Either.Exit (orDie)
import           X.Options.Applicative

data Command = Check View
  deriving (Eq, Show)

main :: IO ()
main = do
  dispatch (safeCommand wardenP) >>= \case
    VersionCommand -> do
      exitSuccess
    RunCommand DryRun c -> do
      print c
      exitSuccess
    RunCommand RealRun c -> do
      r <- orDie renderWardenError $ run c
      mapM_ T.putStrLn r
      exitSuccess

run :: Command -> EitherT WardenError IO [Text]
run (Check v) = (NE.toList . renderCheckResult) <$> check v

wardenP :: Parser Command
wardenP = subparser $
     command' "check" "Run checks over a view." checkP

checkP :: Parser Command
checkP = Check <$> viewP

viewP :: Parser View
viewP = View <$> (strArgument $
     metavar "VIEW"
  <> help "Path to local copy of view.")
