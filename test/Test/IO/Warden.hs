module Test.IO.Warden where

import qualified Data.Text as T

import           Disorder.Core.IO (testIO)

import           P

import           System.IO
import           System.IO.Temp (withTempFile)
import           System.Posix.Directory (getWorkingDirectory)

import           Test.QuickCheck (Testable, Property)

import           Warden.Data
import           Warden.Error

import           X.Control.Monad.Trans.Either

withTestFile :: (ViewFile -> Handle -> IO a) -> IO a
withTestFile a = do
  d <- getWorkingDirectory
  withTempFile d "warden-test-" (\f h -> a (ViewFile f) h)

testWarden :: Testable a => EitherT WardenError IO a -> Property
testWarden = testIO . unsafeWarden

unsafeWarden :: EitherT WardenError IO a -> IO a
unsafeWarden tst = orDie =<< (runEitherT tst)
  where
    orDie (Right a) = pure a
    orDie (Left e) = fail . T.unpack $ renderWardenError e

