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
  testIO . withTestFile $ \fn fh -> do
  T.hPutStr fh cruft
  hFlush fh
  r <- runEitherT . mapEitherT runResourceT $ sanity fn
  pure $ r === Right CheckPassed

prop_insanity_empty :: Property
prop_insanity_empty = testIO . withTestFile $ \fn _ -> unsafeWarden $ do
  r <- sanity fn
  pure $ (CheckFailed $ NE.fromList [SanityCheckFailure EmptyFile]) === r

prop_insanity_symlink :: Property
prop_insanity_symlink = testIO . withTestFile $ \(ViewFile fn) _ -> unsafeWarden $
  let lnk = fn <> "-symlink" in do
  liftIO $ createSymbolicLink fn lnk
  r <- sanity (ViewFile lnk)
  liftIO $ removeLink lnk
  pure $ (CheckFailed $ NE.fromList [SanityCheckFailure IrregularFile]) === r

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
