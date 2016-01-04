{-# LANGUAGE NoImplicitPrelude #-}

module Test.IO.Warden where

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T

import           Disorder.Core.IO (testIO)

import           P

import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>))
import           System.IO
import           System.IO.Temp (withTempFile, withTempDirectory)
import           System.Posix.Directory (getWorkingDirectory)
import           System.Posix.Files (touchFile)

import           Test.QuickCheck (Gen, Testable, Property, forAll, arbitrary)
import           Test.Warden.Arbitrary

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

writeView :: FilePath -> DirTree -> IO ()
writeView r t =
  let dirs = fmap (r </>) $ directoryDirs t
      files = fmap (r </>) $ directoryFiles t in do
  mapM_ (createDirectoryIfMissing True) dirs
  mapM_ touchFile files

withValidView :: Testable a => (View -> EitherT WardenError IO a) -> Property
withValidView a = forAll (arbitrary :: Gen ValidDirTree) $ \(ValidDirTree dt) ->
  withView dt a

withInvalidView :: Testable a => (View -> EitherT WardenError IO a) -> Property
withInvalidView a = forAll (arbitrary :: Gen InvalidDirTree) $ \(InvalidDirTree dt) ->
  withView dt a

withView :: Testable a => DirTree -> (View -> EitherT WardenError IO a) -> Property
withView dt a = testIO . withTempDirectory "." "invalid-view" $ \tmp -> unsafeWarden $
  let (DirTree v _ _) = dt
      v'              = View $ tmp </> unDirName v in do
  liftIO $ writeView tmp dt
  a v'
