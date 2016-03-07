{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.IO.Warden.Check.File where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (runResourceT)

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.IO as T

import           Disorder.Core.IO (testIO)
import           Disorder.Corpus (muppets)
import           Disorder.Core.Property (failWith)

import           P

import           System.IO
import           System.Posix.Files (createSymbolicLink, removeLink)

import           Test.QuickCheck
import           Test.IO.Warden

import           Warden.Check.File
import           Warden.Data

import           X.Control.Monad.Trans.Either

prop_sanity :: Property
prop_sanity = forAll (elements muppets) $ \cruft ->
  testIO . withTestViewFile $ \vf -> do
  let fn = viewFilePath vf
  fh <- openFile fn WriteMode
  T.hPutStr fh cruft
  hFlush fh
  r <- runEitherT . mapEitherT runResourceT $ sanity vf
  pure $ r === Right CheckPassed

prop_insanity_empty :: Property
prop_insanity_empty = testIO . withTestViewFile $ \vf -> unsafeWarden $ do
  liftIO $ writeFile (viewFilePath vf) ""
  r <- sanity vf
  pure $ (CheckFailed $ NE.fromList [SanityCheckFailure EmptyFile]) === r

prop_insanity_symlink :: Property
prop_insanity_symlink = testIO . withTestViewFile $ \vf -> unsafeWarden $
  let lnk = fn <> "-symlink"
      fn = viewFilePath vf in do
  liftIO $ createSymbolicLink fn lnk
  case viewFile lnk of
    Left _ -> pure $ failWith "viewFilePath and viewFile don't appear to be inverses"
    Right vf' -> do
      r <- sanity vf'
      liftIO $ removeLink lnk
      pure $ (CheckFailed $ NE.fromList [SanityCheckFailure IrregularFile]) === r

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
