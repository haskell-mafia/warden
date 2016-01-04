{-# LANGUAGE NoImplicitPrelude #-}

module Test.IO.Warden where

import qualified Data.Text as T

import           Disorder.Core.IO (testIO)

import           P

import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>))
import           System.IO
import           System.IO.Temp (withTempFile, withTempDirectory)
import           System.Posix.Directory (getWorkingDirectory)

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
  mapM_ touch files
  where
    touch fp = writeFile fp ""

withValidDirTree :: Testable a => (View -> IO a) -> Property
withValidDirTree a = forAll (arbitrary :: Gen ValidDirTree) $ \(ValidDirTree dt) ->
  withDirTree dt a

withInvalidDirTree :: Testable a => (View -> IO a) -> Property
withInvalidDirTree a = forAll (arbitrary :: Gen InvalidDirTree) $ \(InvalidDirTree dt) ->
  withDirTree dt a

withDirTree :: Testable a => DirTree -> (View -> IO a) -> Property
withDirTree dt a = testIO . withTempDirectory "." "test-view" $ \tmp -> do
  let (DirTree v _ _) = dt
  let v'              = View $ tmp </> unDirName v
  writeView tmp dt
  a v'
